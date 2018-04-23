function [CIDX,ARI] = run_hydra_experiment_csv(featureCSV,outputDir,varargin)

% function returns estimated subgroups by hydra for clustering
% configurations ranging from K=1 to K=10, or another specified range of
% values. The function returns also the Adjusted Rand Index that was
% calculated across the cross-validation experiments and comparing
% respective clustering solutions.
%
% INPUT
%  
% REQUIRED 
% featureCSV : .csv file containing the input features. (REQUIRED)
%              every column of the file contains values for a feature, with 
%              the exception of the first and last columns. We assume that 
%              the first column contains subject identifying information 
%              while the last column contains label information. First line 
%              of the file should contain header information. Label
%              convention: -1 -> control group - 1 -> pathological group
%              that will be partioned to subgroups 
% outputDir : directory where the output from all folds will be saved (REQUIRED)
%
% OPTIONAL
%
% covCSV : .csv file containing values for different covariates, which
%           will be used to correct the data accordingly (OPTIONAL). Every
%           column of the file contains values for a covariate, with the
%           exception of the first column, which contains subject
%           identifying information. Correction is performed by solving a 
%           solving a least square problem to estimate the respective 
%           coefficients and then removing their effect from the data. The 
%           effect of ALL provided covariates is removed. If no file is 
%           specified, no correction is performed.
%
% NOTE: featureCSV and covCSV files are assumed to have the subjects given
%       in the same order in their rows
%
% C : regularization parameter (positive scalar). smaller values produce 
%     sparser models (OPTIONAL - Default 0.25)
% reg_type : determines regularization type. 1 -> promotes sparsity in the
%            estimated hyperplanes - 2 -> L2 norm (OPTIONAL - Default 1)
% balance : takes into account differences in the number between the two
%           classes. 1-> in case there is mismatch between the number of
%           controls and patient - 0-> otherwise (OPTIONAL - Default 1)
% init : initialization strategy. 0 : assignment by random hyperplanes 
%        (not supported for regression), 1 : pure random assignment, 2: 
%        k-means assignment (default), 3: assignment by DPP random 
%        hyperplanes (not supported for regression), 4: assignment by 
%        k-means to rejection to the binary SVM vector (not supported for 
%        regression)
% iter : number of iterations between estimating hyperplanes, and cluster
%        estimation. Default is 50. Increase if algorithms fails to
%        converge
% numconsensus : number of clustering consensus steps. Default is 20.
%                Increase if algorithm gives unstable clustering results.
% kmin : determines the range of clustering solutions to evaluate
%             (i.e., kmin to kmax). Default  value is 1.
% kmax : determines the range of clustering solutions to evaluate
%             (i.e., kmin to kmax). Default  value is 10.
% kstep: determines the range of clustering solutions to evaluate
%             (i.e., kmin to kmax, with step kstep). Default  value is 1.
% cvfold: number of folds for cross validation. Default value is 10.
% vo : verbose output (i.e., also saves input data to verify that all were 
%      read correctly. Default value is 0
%
% OUTPUT: 
% CIDX: sub-clustering assignments of the disease population (positive
%       class).
% ARI: adjusted rand index measuring the overlap/reproducibility of
%      clustering solutions across folds
%
% NOTE: to compile this function do
% mcc -m  run_hydra_experiment_csv -I ./hydra_package 

if(~isdeployed)
    % include additional files
    addpath ./hydra_package
end

% hydra parameters
% Set as 1 if data is kernelized otherwise 0
% data need to be kernelized for high-dimensional 
params.kernel=0; 

% input parser
p = inputParser;

% required parameters
p.addRequired('featureCSV',@(x)validateattributes(x,{'char'},...
        {'nonempty'}));
p.addRequired('outputDir',@(x)validateattributes(x,{'char'},...
        {'nonempty'}));

% optional parameters
p.addParameter('covCSV',[],@(x)validateattributes(x,{'char'},...
        {'nonempty'}));
p.addParameter('C',0.25); 
p.addParameter('reg_type',1);
p.addParameter('balance',1);
p.addParameter('init',3);
p.addParameter('iter',50);
p.addParameter('numconsensus',20);
p.addParameter('kmin',1);
p.addParameter('kmax',10);
p.addParameter('kstep',1);
p.addParameter('cvfold',10);
p.addParameter('vo',0);

% parse input 
disp('Parsing input...')
parse(p,featureCSV,outputDir,varargin{:});

% create output directory
if (~exist(outputDir,'dir'))
    [status,~,~] = mkdir(outputDir);
    if (status == 0)
        error('run_hydra_experiment_csv:argChk','Cannot create output directory!');
    end
end
    

% assign input parameters to params structure
if(isdeployed)
    if(all(strcmp('C',p.UsingDefaults)==0))
        params.C=str2double(p.Results.C) ; % Regularization parameter
    else
        params.C=p.Results.C ; 
    end
    if(all(strcmp('reg_type',p.UsingDefaults)==0))
        params.reg_type=str2double(p.Results.reg_type); % Regularization type
    else
        params.reg_type=p.Results.reg_type;
    end
    if(all(strcmp('balance',p.UsingDefaults)==0))
        params.balanceclasses=str2double(p.Results.balance); % account for class inbalance
    else
        params.balanceclasses=p.Results.balance;
    end
    if(all(strcmp('init',p.UsingDefaults)==0))
        params.init_type=str2double(p.Results.init); % Algorithm initialization type
    else
        params.init_type=p.Results.init; 
    end
    if(all(strcmp('iter',p.UsingDefaults)==0))
        params.numiter=str2double(p.Results.iter); % number of iterations
    else
        params.numiter=p.Results.iter;
    end
    if(all(strcmp('numconsensus',p.UsingDefaults)==0))
        params.numconsensus=str2double(p.Results.numconsensus); % number of clustering consensus steps
    else
        params.numconsensus=p.Results.numconsensus;
    end
    if(all(strcmp('kmin',p.UsingDefaults)==0))
        params.kmin = str2double(p.Results.kmin) ; % min number of clusters to consider
    else
        params.kmin = p.Results.kmin ; 
    end
    if(all(strcmp('kmax',p.UsingDefaults)==0))
        params.kmax = str2double(p.Results.kmax) ; % max number of clusters to consider
    else
        params.kmax = p.Results.kmax ;
    end
    if(all(strcmp('kstep',p.UsingDefaults)==0))
        params.kstep = str2double(p.Results.kstep) ; % step defining the range clustering solutions to consider
    else
        params.kstep = p.Results.kstep ;
    end
    if(all(strcmp('cvfold',p.UsingDefaults)==0))
        params.cvfold = str2double(p.Results.cvfold) ; % number of folds in k-fold cross validation
    else
        params.cvfold = p.Results.cvfold ; 
    end
    if(all(strcmp('vo',p.UsingDefaults)==0))
        params.vo = str2double(p.Results.vo) ; % verbose output or no
    else
        params.vo = p.Results.vo ; 
    end
else
    params.C=p.Results.C ; % Regularization parameter
    params.reg_type=p.Results.reg_type; % Regularization type
    params.balanceclasses=p.Results.balance; % account for class inbalance
    params.init_type=p.Results.init; % Algorithm initialization type
    params.numiter=p.Results.iter; % number of iterations
    params.numconsensus=p.Results.numconsensus; % number of clustering consensus steps
    params.kmin = p.Results.kmin ; % min number of clusters to consider
    params.kmax = p.Results.kmax ; % max number of clusters to consider
    params.kstep = p.Results.kstep ; % step defining the range clustering solutions to consider
    params.cvfold = p.Results.cvfold ; % number of folds in k-fold cross validation    
    params.vo = p.Results.vo ; % verbose output or no    
end

% confirm validity of optional input arguments
validateFcn_reg_type = @(x) (x==1) || (x == 2);
validateFcn_balance = @(x) (x==0) || (x == 1);
validateFcn_init = @(x) (x==0) || (x == 1) || (x==2) || (x == 3) || (x == 4);
validateFcn_iter = @(x) isscalar(x) && (x>0) && (mod(x,1)==0);
validateFcn_consensus = @(x) isscalar(x) && (x>0) && (mod(x,1)==0);
validateFcn_kmin = @(x) isscalar(x) && (x>0) && (mod(x,1)==0);
validateFcn_kmax = @(x,y) isscalar(x) && (x>0) && (mod(x,1)==0) && (x>y);
validateFcn_kstep = @(x,y,z) isscalar(x) && (x>0) && (mod(x,1)==0) && (x+y<z);
validateFcn_cvfold = @(x) isscalar(x) && (x>0) && (mod(x,1)==0);
validateFcn_vo = @(x) (x==0) || (x == 1);

if(~validateFcn_reg_type(params.reg_type))
    error('run_hydra_experiment_csv:argChk','Input regularization type (reg_type) should be either 1 or 2!');
end
if(~validateFcn_balance(params.balanceclasses))
    error('run_hydra_experiment_csv:argChk','Input balance classes (balance) should be either 1 or 2!');
end
if(~validateFcn_init(params.init_type))
    error('run_hydra_experiment_csv:argChk','Initialization type can be either 0, 1, 2, 3, or 4!');
end
if(~validateFcn_iter(params.numiter))
    error('run_hydra_experiment_csv:argChk','Number of iterations should be a positive integer!');
end
if(~validateFcn_consensus(params.numconsensus))
    error('run_hydra_experiment_csv:argChk','Number of clustering consensus steps should be a positive integer!');
end
if(~validateFcn_kmin(params.kmin))
    error('run_hydra_experiment_csv:argChk','Minimum number of clustering solutions to consider should be a positive integer!');
end
if(~validateFcn_kmax(params.kmax,params.kmin))
    error('run_hydra_experiment_csv:argChk','Maximum number of clustering solutions to consider should be a positive integer that is greater than the minimum number of clustering solutions!');
end
if(~validateFcn_kstep(params.kstep,params.kmin,params.kmax))
    error('run_hydra_experiment_csv:argChk','Step number of clustering solutions to consider should be a positive integer that is between the minimun and maximum number of clustering solutions!');
end
if(~validateFcn_cvfold(params.cvfold))
    error('run_hydra_experiment_csv:argChk','Number of folds for cross-validation should be a positive integer!');
end
if(~validateFcn_vo(params.vo))
    error('run_hydra_experiment_csv:argChk','VO parameter should be either 0 or 1!');
end

disp('Done');
disp('HYDRA runs with the following parameteres');
disp(['featureCSV: ' featureCSV]);
disp(['OutputDir: ' outputDir]);
disp(['covCSV:' p.Results.covCSV])
disp(['C: ' num2str(params.C)]);
disp(['reg_type: ' num2str(params.reg_type)]);
disp(['balanceclasses: ' num2str(params.balanceclasses)]);
disp(['init_type: ' num2str(params.init_type)]);
disp(['numiter: ' num2str(params.numiter)]);
disp(['numconsensus: ' num2str(params.numconsensus)]);
disp(['kmin: ' num2str(params.kmin)]);
disp(['kmax: ' num2str(params.kmax)]);
disp(['kstep: ' num2str(params.kstep)]);
disp(['cvfold: ' num2str(params.cvfold)]);
disp(['vo: ' num2str(params.vo)]);

% csv with features
fname=p.Results.featureCSV;
if (~exist(fname,'file'))
    error('run_hydra_experiment_csv:argChk','Input feature .csv file does not exist');
end

% csv with features
covfname=p.Results.covCSV;
if(~isempty(covfname))
    if(~exist(covfname,'file'))
        error('run_hydra_experiment_csv:argChk','Input covariate .csv file does not exist');
    end
end

% input data
% assumption is that the first column contains IDs, and the last contains
% labels
disp('Loading features...');
input=readtable(fname);
ID=input{:,1};
XK=input{:,2:end-1};
Y=input{:,end};

% z-score imaging features
XK=zscore(XK);
disp('Done');

% input covariate information if necesary
if(~isempty(covfname))
    disp('Loading covariates...');
    covardata = readtable(covfname) ;
    IDcovar = covardata{:,1};
    covar = covardata{:,2:end};
    covar = zscore(covar);
    disp('Done');
end
        
% NOTE: we assume that the imaging data and the covariate data are given in
% the same order. No test is performed to check that. By choosing to have a
% verbose output, you can have access to the ID values are read by the
% software for both the imaging data and the covariates

% verify that we have covariate data and imaging data for the same number
% of subjects
if(~isempty(covfname))
    if(size(covar,1)~=size(XK,1))
        error('run_hydra_experiment_csv:argChk','The feature .csv and covariate .csv file contain data for different number of subjects');
    end
end

% residualize covariates if necessary
if(~isempty(covfname))
    disp('Residualize data...');
    [XK0,~]=GLMcorrection(XK,Y,covar,XK,covar);
    disp('Done');
else
    XK0=XK; 
end

% for each realization of cross-validation
clustering=params.kmin:params.kstep:params.kmax;
part=make_xval_partition(size(XK0,1),params.cvfold); %Partition data to 10 groups for cross validation
% for each fold of the k-fold cross-validation
disp('Run HYDRA...');
for f=1:params.cvfold
    % for each clustering solution
    for kh=1:length(clustering)
        params.k=clustering(kh);
        disp(['Applying HYDRA for ' num2str(params.k) ' clusters. Fold: ' num2str(f) '/' num2str(params.cvfold)]);
        model=hydra_03092016(XK0(part~=f,:),Y(part~=f,:),[],params);
        YK{kh}(part~=f,f)=model.Yhat;
    end
end
disp('Done');

disp('Estimating clustering stabilitiy...')
% estimate cluster stability for the cross-validation experiment
ARI = zeros(length(clustering),1);
for kh=1:length(clustering)
    tmp=cv_cluster_stability(YK{kh}(Y~=-1,:));
    ARI(kh)=tmp(1);
end
disp('Done')

disp('Estimating final consensus group memberships...')
% Computing final consensus group memberships
CIDX=-ones(size(Y,1),length(clustering)); %variable that stores subjects in rows, and cluster memberships for the different clustering solutions in columns
for kh=1:length(clustering)
    CIDX(Y==1,kh)=consensus_clustering(YK{kh}(Y==1,:),clustering(kh));
end
disp('Done')

disp('Saving results...')
if(params.vo==0)
    save([outputDir '/HYDRA_results.mat'],'ARI','CIDX','clustering','ID');
else
    save([outputDir '/HYDRA_results.mat'],'ARI','CIDX','clustering','ID','XK','Y','covar','IDcovar');
end
disp('Done')
