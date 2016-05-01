#This file includes the original and scaled DTLZ1 to DTLZ4

# function [Output,Boundary,Coding] <- P_objective(Operation,Problem,M,Input)
P_objective <- function(Operation, Problem, M, Input){
    m <- regexpr("[a-zA-Z]+", Problem)
    value <- regmatches(Problem, m)
    if (value == "DTLZ")
            List[Output,Boundary,Coding] <- P_DTLZ(Operation,Problem,M,Input)
    else if (value == 'SDTLZ')
            List[Output,Boundary,Coding] <- P_SDTLZ(Operation,Problem,M,Input)
    else
            stop(Problem,' Does Not Exist')
    
    return (list(Output, Boundary, Coding))
}

# This global variable for the function below
K_DTLZ <- 0
# function [Output,Boundary,Coding] <- P_DTLZ(Operation,Problem,M,Input)
P_DTLZ <- function(Operation, Problem, M, Input){
    Boundary <- NaN; 
    Coding <- NaN
    if (Operation == "init"){
            k <- find_last_alphabet_index(Problem)
            K_DTLZ <- c(5, 10, 10, 10, 10, 10, 20)
            K_DTLZ <- K_DTLZ[as.numeric(substr(Problem, k+1, length(Problem)))]

            D <- M+K-1
            MaxValue   <- rep(1,D)
            MinValue   <- rep(0,D)
            # wrote util for rand
            Population <- rand(Input,D)
            Population <- Population.*repmat(MaxValue,Input,1)+(1-Population).*repmat(MinValue,Input,1)

            Output   <- Population
            Boundary <- [MaxValue;MinValue]
            Coding   <- 'Real'
    }
        #Objective Function Evaluation
        case 'value'
            Population    <- Input
            FunctionValue <- zeros(size(Population,1),M)
            switch Problem
                case 'DTLZ1'
                    g <- 100*(K+sum((Population(:,M:length(Population))-0.5).^2-cos(20.*pi.*(Population(:,M:length(Population))-0.5)),2))
                    for (i in 1 : M){
                        FunctionValue(:,i) <- 0.5.*prod(Population(:,1:M-i),2).*(1+g)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*(1-Population(:,M-i+1))
                        }
                    }
                case 'DTLZ2'
                    g <- sum((Population(:,M:length(Population))-0.5).^2,2)
                    for (i in 1 : M){
                        FunctionValue(:,i) <- (1+g).*prod(cos(0.5.*pi.*Population(:,1:M-i)),2)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*sin(0.5.*pi.*Population(:,M-i+1))
                        }
                    }
                case 'DTLZ3'
                    g <- 100*(K+sum((Population(:,M:length(Population))-0.5).^2-cos(20.*pi.*(Population(:,M:length(Population))-0.5)),2))
                    for (i in 1 : M){
                        FunctionValue(:,i) <- (1+g).*prod(cos(0.5.*pi.*Population(:,1:M-i)),2)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*sin(0.5.*pi.*Population(:,M-i+1))
                        }
                    }
                case 'DTLZ4'
                    Population(:,1:M-1) <- Population(:,1:M-1).^100
                    g <- sum((Population(:,M:length(Population))-0.5).^2,2)
                    for (i in 1 : M){
                        FunctionValue(:,i) <- (1+g).*prod(cos(0.5.*pi.*Population(:,1:M-i)),2)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*sin(0.5.*pi.*Population(:,M-i+1))
                        }
                    }
            }
            Output <- FunctionValue
        #Sample True PFs
        case 'true'
            switch Problem
                case 'DTLZ1'
                    Population in T_uniform(Input,M)){){
                    Population <- Population/2
                case {'DTLZ2','DTLZ3','DTLZ4'}
                    Population in T_uniform(Input,M)){){
                    for (i in 1 : size(Population,1)){
                    	Population(i,:) <- Population(i,:)./norm(Population(i,:))
                    }
            }
            Output <- Population
    }
}


function [Output,Boundary,Coding] <- P_SDTLZ(Operation,Problem,M,Input)
    persistent K
    persistent F
    Boundary <- NaN; Coding <- NaN
    switch Operation
        #Population Initialization
        case 'init'
            k <- find(!isstrprop(Problem,'digit'),1,'last')
            K <- [5 10 10 10 10 10 20]
            K <- K(str2double(Problem(k+1:length(Problem))))
            F <- [10 10 10 10 10 5 4 3 2 2]
            F <- F(M)

            D <- M+K-1
            MaxValue   <- ones(1,D)
            MinValue   <- zeros(1,D)
            Population <- rand(Input,D)
            Population <- Population.*repmat(MaxValue,Input,1)+(1-Population).*repmat(MinValue,Input,1)

            Output   <- Population
            Boundary <- [MaxValue;MinValue]
            Coding   <- 'Real'
        #Objective Function Evaluation
        case 'value'
            Population    <- Input
            FunctionValue <- zeros(size(Population,1),M)
            switch Problem
                case 'SDTLZ1'
                    g <- 100*(K+sum((Population(:,M:length(Population))-0.5).^2-cos(20.*pi.*(Population(:,M:length(Population))-0.5)),2))
                    for (i in 1 : M){
                        FunctionValue(:,i) <- 0.5.*prod(Population(:,1:M-i),2).*(1+g)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*(1-Population(:,M-i+1))
                        }
                    }
                case 'SDTLZ2'
                    g <- sum((Population(:,M:length(Population))-0.5).^2,2)
                    for (i in 1 : M){
                        FunctionValue(:,i) <- (1+g).*prod(cos(0.5.*pi.*Population(:,1:M-i)),2)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*sin(0.5.*pi.*Population(:,M-i+1))
                        }
                    }
                case 'SDTLZ3'
                    g <- 100*(K+sum((Population(:,M:length(Population))-0.5).^2-cos(20.*pi.*(Population(:,M:length(Population))-0.5)),2))
                    for (i in 1 : M){
                        FunctionValue(:,i) <- (1+g).*prod(cos(0.5.*pi.*Population(:,1:M-i)),2)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*sin(0.5.*pi.*Population(:,M-i+1))
                        }
                    }
                case 'SDTLZ4'
                    Population(:,1:M-1) <- Population(:,1:M-1).^100
                    g <- sum((Population(:,M:length(Population))-0.5).^2,2)
                    for (i in 1 : M){
                        FunctionValue(:,i) <- (1+g).*prod(cos(0.5.*pi.*Population(:,1:M-i)),2)
                        if (i > 1){
                            FunctionValue(:,i) <- FunctionValue(:,i).*sin(0.5.*pi.*Population(:,M-i+1))
                        }
                    }
            }
            Output <- FunctionValue.*repmat((F.^(0:M - 1)), [size(FunctionValue,1) 1])
        #Sample True PFs
        case 'true'
            switch Problem
                case 'SDTLZ1'
                    Population in T_uniform(Input,M)){){
                    Population <- Population/2
                case {'SDTLZ2','SDTLZ3','SDTLZ4'}
                    Population in T_uniform(Input,M)){){
                    for (i in 1 : size(Population,1)){
                    	Population(i,:) <- Population(i,:)./norm(Population(i,:))
                    }
            }
            Output <- Population.*repmat((F.^(0:M - 1)), [size(Population,1) 1])
    }
}

function W in T_uniform(k,M)){){
    H <- floor((k*prod(1:M-1))^(1/(M-1)))
    while (nchoosek(H+M-1,M-1) > in  k && H > 0){
        H <- H-1
    }
    if (nchoosek(H+M,M-1) <= 2*k || H == 0){
        H <- H+1
    }
    k <- nchoosek(H+M-1,M-1)
    Temp <- nchoosek(1:H+M-1,M-1)-repmat(0:M-2,nchoosek(H+M-1,M-1),1)-1
    W <- zeros(k,M)
    W(:,1) <- Temp(:,1)-0
    for (i in 2 : M-1){
        W(:,i) <- Temp(:,i)-Temp(:,i-1)
    }
    W(:,length(Temp)) <- H-Temp(:,length(Temp))
    W <- W/H
}

function W <- T_repeat(k,M)
    if (M > 1){
        k <- (ceil(k^(1/M)))^M
        Temp <- 0:1/(k^(1/M)-1):1
        code <- '[c1'
        for (i in 2 : M){
            code <- [code,',c',num2str(i)]
        }
        code <- [code,'] <- ndgrid(Temp);']
        eval(code)
        code <- 'W <- [c1(:)'
        for (i in 2 : M){
            code <- [code,',c',num2str(i),'(:)']
        }
        code <- [code,'];']
        eval(code)
    } else {
        W <- [0:1/(k-1):1]'
    }
}

function FunctionValue <- T_sort(FunctionValue)
    Choose <- true(1,size(FunctionValue,1))
    [!,rank] <- sortrows(FunctionValue)
    for (i in rank'){
        for (j in rank(find(rank in  in i)+1:length(rank))'){
            if (Choose(j)){
                k <- 1
                for (m in 2 : size(FunctionValue,2)){
                    if (FunctionValue(i,m) > FunctionValue(j,m)){
                        k <- 0
                        break
                    }
                }
                if (k == 1){
                    Choose(j) <- false
                }
            }
        }
    }
    FunctionValue <- FunctionValue(Choose,:)
}
