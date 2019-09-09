clear all

%% Life history [survival(px) fertility(mx)]

LH_smooth = [0.878640777	0
    0.975524328	0
    0.958846758	0
    0.981477577	0
    0.985106889	0
    0.988432128	0
    0.991597517	0
    0.992417212	0
    0.992162504	0
    0.991970246	0
    0.99191801	0
    0.99317639	0
    0.99293438	0
    0.993975414	0
    0.993356343	0
    0.993924401	0
    0.991934075	0
    0.991698168	0.215588864
    0.991888417	0.279545281
    0.992276379	0.319357721
    0.990682084	0.356643651
    0.990231766	0.377941045
    0.988344805	0.388560008
    0.986524785	0.373160653
    0.986912456	0.37408373
    0.987930685	0.362389637
    0.989026728	0.355580113
    0.99109657	0.339946585
    0.991010348	0.351766369
    0.991248037	0.33597045
    0.991365781	0.329341609
    0.991484185	0.32596534
    0.98990882	0.321961915
    0.990586769	0.313941598
    0.988975014	0.304987517
    0.987166671	0.309283579
    0.985995799	0.291419246
    0.98672687	0.284918725
    0.988619615	0.263062881
    0.989688774	0.262263331
    0.988173095	0.241293889
    0.990226305	0.226360516
    0.987777045	0.204336283
    0.985725763	0.165865105
    0.984035485	0.122914654
    0.987859015	0.08224026
    0.985077671	0.053587395
    0.987168001	0.026431301
    0.982274838	0.019288444
    0.981159777	0.011788444
    0.979259325	0.006308992
    0.978039812	0
    0.97214181	0
    0.9677655	0
    0.970072002	0
    0.970876198	0
    0.970934969	0
    0.974353773	0
    0.96853167	0
    0.957522179	0
    0.952102599	0
    0.948921876	0
    0.945161192	0
    0.952547952	0
    0.942805563	0
    0.943831204	0
    0.924244997	0
    0.915742582	0
    0.903276457	0
    0.907667	0
    0.888000444	0];

lx(1) = 1;                          % survivorship ~ cumulative survival probability
lx(2:72) = cumprod(LH_smooth(:,1)); % FIG S1C
e0 = sum(lx);                       % life expectancty at birth
mx = LH_smooth(:,2) ./ 2.05;        % maternity function - only counts female births (assuming SRB = 1.05)
TFR = sum(LH_smooth(:,2));          % total fertility rate (survival-conditioned lifetime fertility)

%% Strength and Skill ontogenies (after Gurven and Kaplan 2006)

% FIG S1A
xmax = 71;                              % maximum age in model (70 y)
Ssc = Seq(1:xmax)./max(Seq(1:xmax));    % Strength age profile
Ksc = Keq(1:xmax)./max(Keq(1:xmax));    % Skill age profile

% Requirements for strength (a) and skill (b)
a1 = 0.1; b1 = 0.1;                     % a1 = 0.3; b1 = 0.1;
a2 = 0.7; b2 = 0.1;                     % a2 = 0.7; b2 = 0.1;
a3 = 0.1; b3 = 0.7;                     % a3 = 0.3; b3 = 0.7;
a4 = 0.7; b4 = 0.7;                     % a4 = 0.7; b4 = 0.7;
% Baseline prod under S/K 
Prod_SK = [(Ssc.^a1).*(Ksc.^b1);        % FIG S1B                            
    (Ssc.^a2).*(Ksc.^b2); (Ssc.^a3).*(Ksc.^b3);(Ssc.^a4).*(Ksc.^b4)];
% Scaled production (relative to maximum = 1)
Prod_SK_sc = Prod_SK ./ repmat(max(Prod_SK')',1,71);
% Food contribution (survival-discounted reflecting population age structure)
Px_SK = Prod_SK .* repmat(lx(1:71),4,1);    
Pmax_SK = max(Prod_SK');                        % peak production
PT_SK = sum(Px_SK');                            % total lifetime production
Px_SK_sc= Prod_SK_sc .* repmat(lx(1:71),4,1);   % scaled production (for comparison)
Pmax_SK_sc = max(Prod_SK_sc');                  % peak scaled production (Px = 1)   
PT_SK_sc = sum(Px_SK_sc');                      % total scaled production (units = peak production-years)

% Age of peak production
for isk = 1:4
    Pmax(isk) = find(Prod_SK_sc(isk,:)==max(Prod_SK_sc(isk,:)));
end

% Lifetime Production (P_T) under different strength/skill (S/K) requirements
iis = 0.1:0.05:1.2;                                 % varying strength/skill requirments (\alpha, \beta)
for is = 1:length(iis)
    PT_SvarH(:,is) = Ssc.^iis(is) .* (Ksc.^0.7);    % High skill, vary strength
    PT_KvarH(:,is) = Ssc.^0.7 .* (Ksc.^iis(is));    % High strength, vary skill
    PT_SvarL(:,is) = Ssc.^iis(is) .* (Ksc.^0.1);    % Low skill, vary strength
    PT_KvarL(:,is) = Ssc.^0.1 .* (Ksc.^iis(is));    % Low strength, vary skill
end

% Continuously varying strength/skill requirments (for illustration)
% FIG S2
isks = 0.1:0.05:1.2;
for iss = 1:length(isks)
    for isk = 1:length(isks)
       Px_isks(:,iss,isk) = Ssc.^isks(iss) .* Ksc.^isks(isk); 
       Px_isks_sc(:,iss,isk) = Px_isks(:,iss,isk) ./ max(Px_isks(:,iss,isk)); 
    end
end

%% Instruction effects on skills ontogeny (x,theta,a,b,t)

bmin = 11;          % Earliest teacher onset - age 10
bmax = 65;          % Oldest teacher onset - age 64
amax = 31;          % oldest pupil onset - age 30
tmax = 25;          % longest teaching duration - 25 y
xmax = 71;          % oldest age (T) - 70 y
a = 1:amax;         % pupil onset
b = 1:bmax;         % teacher onset
theta = 0:0.01:1;   % pupil boost
phi = 0:0.01:1;     % teacher handicap ~ zero at iphi = 1 (Px* = (1-phi)*Px);
t = 1:tmax;         % Max duration (if b+t < xmax)

% Rate of learning (dK/dx)
dKdx = Ksc(2:end) - Ksc(1:end-1);               % slope of skill(age)
dK_at_Kb = repmat(dKdx',1,101,bmax,amax,tmax);  % slope of skill with only teacher skill effects
K_at_Kb = zeros(xmax,101,bmax,amax,tmax);       % cumulative skill with only teacher skill effects
dK_at_Ka = repmat(dKdx',1,101,bmax,amax,tmax);  % slope of skill with teacher and pupil skill effects
K_at_Ka = zeros(xmax,101,bmax,amax,tmax);       % cumulative skill with teacher and pupil skill effects

% Pupil skills ontogeny - FIG 2
for it = 1:tmax                                                         % Duration of instruction (t)
    for ia = 1:amax                                                     % Pupil age at onset (a)
        for ib = a(ia):bmax                                             % Teacher age at onset (b)
            b(ib) = ib;                                              	
            tK = min(length(b(ib):b(ib)+t(it))-1,length(b(ib):xmax)); 	
            if ia+tK <=xmax                                             % Caps duration at teacher age limit (xmax = 70)
                for itheta = 1:101
                    theta(itheta) = (itheta-1) * 0.01;                 	% pupil boost (multiplier on slope of skill(ag)
                    % baseline skills ontogeny: pupil boost(theta) discounted (~ teacher survival, teacher skill)
                    K_at_Kb(a(ia):a(ia)+tK-1,itheta,ib,ia,it) = dKdx(a(ia):a(ia)+tK-1) +...   
                        Ksc(b(ib):b(ib)+tK-1) .* lx(b(ib):b(ib)+tK-1) .* theta(itheta) .* dKdx(a(ia):a(ia)+tK-1);    
                    % baseline skills ontogeny: pupil boost(theta) discounted (~ teacher survival, teacher skill, pupil skill)
                    dK_at_Ka(a(ia):a(ia)+tK-1,itheta,ib,ia,it) = dKdx(a(ia):a(ia)+tK-1) +...   
                        Ksc(a(ia):a(ia)+tK-1) .* Ksc(b(ib):b(ib)+tK-1) .* lx(b(ib):b(ib)+tK-1) .* theta(itheta) .* dKdx(a(ia):a(ia)+tK-1);
                end
            end
        end
    end
end
% Cumulative skill at each age assuming only teacher skill effects
K_at_Kb(2:xmax,:,:,:,:) = cumsum(dK_at_Kb(1:xmax-1,:,:,:,:));           % Cumulative skills ontogeny
K_at_Kb(K_at_Kb>1) = 1;                                                	% Cap K at 100% baseline max
K_at_Kb(59:xmax,:,:,:,:) = repmat(Ksc(59:end)',1,101,bmax,amax,tmax);   % K after b at baseline
% Cumulative skill at each age assuming teacher and pupil skill effects
K_at_Ka(2:xmax,:,:,:,:) = cumsum(dK_at_Ka(1:xmax-1,:,:,:,:));        	% Cumulative skills ontogeny
K_at_Ka(K_at_Ka>1) = 1;                                             	% Cap K at 100% baseline max
K_at_Ka(59:xmax,:,:,:,:) = repmat(Ksc(59:end)',1,101,bmax,amax,tmax);   % K after b at baseline
% Benefit of pedagogy for skills ontogeny
Kb_diff = K_at_Kb - repmat(Ksc',1,101,bmax,amax,tmax);
Ka_diff = K_at_Ka - repmat(Ksc',1,101,bmax,amax,tmax);

% Age at mastery (min age s.t. Kx = max(Kx))        % FIG S3
for it = 1:tmax
    for ia = 1:amax
        for ib = bmin:bmax
            for itheta = 1:101
                xKmax_b(itheta,ib,ia,it) = min(find(K_at_Kb(:,itheta,ib,ia,it)==1));    % teacher skill effects             
                xKmax_a(itheta,ib,ia,it) = min(find(K_at_Ka(:,itheta,ib,ia,it)==1));    % teacher and pupil skill effects
            end
        end
    end
end

% Optimal teacher age for pupil skills ontogeny (*NOT* acccounting for production tradeoffs) - FIG S4
bopt_Kb = zeros(101,amax,tmax);
bopt_Ka = zeros(101,amax,tmax);
for itheta = 2:101
    for it = 1:tmax
        for ia = 1:amax
            % teacher skill effects
            dummy_bopt = find(squeeze(xKmax_b(itheta,ib,:,it))==min(squeeze(xKmax_b(itheta,ib,:,it))));
            bopt_Kb1(itheta,ia,it) = min(find(squeeze(xKmax_b(itheta,:,ia,it))==min(squeeze(xKmax_b(itheta,:,ia,it)))));
            bopt_Kb2(itheta,ia,it) = max(find(squeeze(xKmax_b(itheta,:,ia,it))==min(squeeze(xKmax_b(itheta,:,ia,it)))));
            % teacher and pupil skill effects
            dummy_bopt = find(squeeze(xKmax_a(itheta,ib,:,it))==min(squeeze(xKmax_a(itheta,ib,:,it))));
            bopt_Ka1(itheta,ia,it) = min(find(squeeze(xKmax_a(itheta,:,ia,it))==min(squeeze(xKmax_a(itheta,:,ia,it)))));
            bopt_Ka2(itheta,ia,it) = max(find(squeeze(xKmax_a(itheta,:,ia,it))==min(squeeze(xKmax_a(itheta,:,ia,it)))));
        end
    end
end

save Instruction_setup_Kab                      % save results
save Instruction_setup_2 K_at_Kb K_at_Kb -v7.3  % save skills matrix (large memory requirement)
save Instruction_setup_2_K_at_Ka K_at_Ka -v7.3  % save skills matrix (large memory requirement)

%% Lifetime production with teacher skill effect on pedagogy (no student skill effect)
% FIGS 3,4,S5,S6

% Teacher handicap
phi = 0.01*[1:101]-0.01;
% Pupil boost
theta = phi;
% Age at onset for teacher (b) and pupil (a)
b = 1:bmax; a = 1:amax;

% Low Strength (S), Low Skill (K)
PT_matLL_Kb = zeros(length(phi),length(theta),length(b),length(a));
t = 1:25;it = 10;                       % assuming 10y instruction for illustration
alpha1 = 0.1; beta1 = 0.1;          	% strength(alpha1) and skill (beta1) requirements
Kx_mat = K_at_Kb .^ beta1;              % Skill effect on production
for iphi = 1:length(phi)                % Teacher handicap (proportion of baseline teacher production)
    for itheta = 1:length(theta)        % Pupil boost (multiplier on slope of skill(age))
        for ib = bmin:bmax              % Teacher age at onset (b)
            for ia = 1:amax             % Pupil age at onset (a)
                q = ib-ia;  % age difference (teacher - pupil)
                d(ia,ib,itheta,it) = min(t(it),xKmax_b(itheta,ib,ia,it)-a(ia));
                % Production without teacher handicap
                Px_mat(1:b(ib)-1) = lx(1:b(ib)-1) .* (Ssc(1:b(ib)-1).^alpha1) .*...
                    (Kx_mat(1:b(ib)-1,itheta,ib,ia,it))';
                % Production with teacher handicap
                Px_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) = (1-phi(iphi)) .* lx(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) .*...
                    Ssc(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)).^alpha1 .*...
                    Kx_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax),itheta,ib,ia,it)'.^beta1;
                % Residual boost ~ Teaching ends when pupil attains mastery at xKmax
                if b(ib)+d(ia,ib,itheta)+1 < xmax
                Px_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax) = lx(b(ib)+d(ia,ib,itheta,it)+1:xmax) .* (Ssc(b(ib)+d(ia,ib,itheta,it)+1:xmax).^alpha1) .*...
                    (Kx_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax,itheta,ib,ia,it))';
                else
                end
                % Lifetime production (survival-discounted ~ age structure of food contributions
                PT_matLL_Kb(iphi,itheta,ib,ia) = sum(Px_mat);
            end
        end
    end   
end
save PxLL_Kb PT_matLL_Kb -v7.3
% clear PT_matLL_Kb Kx_mat

% High Strength (S), Low Skill (K)
PT_matHL_Kb = zeros(length(phi),length(theta),length(b),length(a));    %,length(t));      % (theta, phi, b, a, t)
t = 1:25;it = 10;                       % assuming 10y instruction for illustration
alpha1 = 0.7; beta1 = 0.1;          	% strength(alpha1) and skill (beta1) requirements
Kx_mat = K_at_Kb .^ beta1;              % Skill effect on production
for iphi = 1:length(phi)                % Teacher handicap (proportion of baseline teacher production)
    for itheta = 1:length(theta)        % Pupil boost (multiplier on slope of skill(age))
        for ib = bmin:bmax              % Teacher age at onset (b)
            for ia = 1:amax             % Pupil age at onset (a)
                q = ib-ia;  % age difference (teacher - pupil)
                d(ia,ib,itheta,it) = min(t(it),xKmax_b(itheta,ib,ia,it)-a(ia));
                % Production without teacher handicap
                Px_mat(1:b(ib)-1) = lx(1:b(ib)-1) .* (Ssc(1:b(ib)-1).^alpha1) .*...
                    (Kx_mat(1:b(ib)-1,itheta,ib,ia,it))';
                % Production with teacher handicap
                Px_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) = (1-phi(iphi)) .* lx(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) .*...
                    Ssc(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)).^alpha1 .*...
                    Kx_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax),itheta,ib,ia,it)'.^beta1;
                % Residual boost ~ Teaching ends when pupil attains mastery at xKmax
                if b(ib)+d(ia,ib,itheta)+1 < xmax
                Px_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax) = lx(b(ib)+d(ia,ib,itheta,it)+1:xmax) .* (Ssc(b(ib)+d(ia,ib,itheta,it)+1:xmax).^alpha1) .*...
                    (Kx_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax,itheta,ib,ia,it))';
                else
                end
                % Lifetime production (survival-discounted ~ age structure of food contributions
                PT_matHL_Kb(iphi,itheta,ib,ia) = sum(Px_mat);
            end
        end
    end   
end
save PxHL_Kb PT_matHL_Kb -v7.3
% clear PT_matHL_Kb Kx_mat               

% Low Strength (S), High Skill (K)
PT_matLH_Kb = zeros(length(phi),length(theta),length(b),length(a));    %,length(t));      % (theta, phi, b, a, t)
t = 1:25;it = 10;                       % assuming 10y instruction for illustration
alpha1 = 0.1; beta1 = 0.7;          	% strength(alpha1) and skill (beta1) requirements
Kx_mat = K_at_Kb .^ beta1;              % Skill effect on production
for iphi = 1:length(phi)                % Teacher handicap (proportion of baseline teacher production)
    for itheta = 1:length(theta)        % Pupil boost (multiplier on slope of skill(age))
        for ib = bmin:bmax              % Teacher age at onset (b)
            for ia = 1:amax             % Pupil age at onset (a)
                q = ib-ia;  % age difference (teacher - pupil)
                d(ia,ib,itheta,it) = min(t(it),xKmax_b(itheta,ib,ia,it)-a(ia));
                % Production without teacher handicap
                Px_mat(1:b(ib)-1) = lx(1:b(ib)-1) .* (Ssc(1:b(ib)-1).^alpha1) .*...
                    (Kx_mat(1:b(ib)-1,itheta,ib,ia,it))';
                % Production with teacher handicap
                Px_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) = (1-phi(iphi)) .* lx(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) .*...
                    Ssc(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)).^alpha1 .*...
                    Kx_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax),itheta,ib,ia,it)'.^beta1;
                % Residual boost ~ Teaching ends when pupil attains mastery at xKmax
                if b(ib)+d(ia,ib,itheta)+1 < xmax
                Px_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax) = lx(b(ib)+d(ia,ib,itheta,it)+1:xmax) .* (Ssc(b(ib)+d(ia,ib,itheta,it)+1:xmax).^alpha1) .*...
                    (Kx_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax,itheta,ib,ia,it))';
                else
                end
                % Lifetime production (survival-discounted ~ age structure of food contributions
                PT_matLH_Kb(iphi,itheta,ib,ia) = sum(Px_mat);
            end
        end
    end   
end
save PxLH_Kb PT_matLH_Kb -v7.3
% clear PT_matLH_Kb Kx_mat

% High Strength (S), High Skill (K)
PT_matHH_Kb = zeros(length(phi),length(theta),length(b),length(a));    %,length(t));      % (theta, phi, b, a, t)
t = 1:25;it = 10;                       % assuming 10y instruction for illustration
alpha1 = 0.7; beta1 = 0.7;          	% strength(alpha1) and skill (beta1) requirements
Kx_mat = K_at_Kb .^ beta1;              % Skill effect on production
for iphi = 1:length(phi)                % Teacher handicap (proportion of baseline teacher production)
    for itheta = 1:length(theta)        % Pupil boost (multiplier on slope of skill(age))
        for ib = bmin:bmax              % Teacher age at onset (b)
            for ia = 1:amax             % Pupil age at onset (a)
                q = ib-ia;  % age difference (teacher - pupil)
                d(ia,ib,itheta,it) = min(t(it),xKmax_b(itheta,ib,ia,it)-a(ia));
                % Production without teacher handicap
                Px_mat(1:b(ib)-1) = lx(1:b(ib)-1) .* (Ssc(1:b(ib)-1).^alpha1) .*...
                    (Kx_mat(1:b(ib)-1,itheta,ib,ia,it))';
                % Production with teacher handicap
                Px_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) = (1-phi(iphi)) .* lx(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)) .*...
                    Ssc(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax)).^alpha1 .*...
                    Kx_mat(b(ib):min(b(ib)+d(ia,ib,itheta,it),xmax),itheta,ib,ia,it)'.^beta1;
                % Residual boost ~ Teaching ends when pupil attains mastery at xKmax
                if b(ib)+d(ia,ib,itheta)+1 < xmax
                Px_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax) = lx(b(ib)+d(ia,ib,itheta,it)+1:xmax) .* (Ssc(b(ib)+d(ia,ib,itheta,it)+1:xmax).^alpha1) .*...
                    (Kx_mat(b(ib)+d(ia,ib,itheta,it)+1:xmax,itheta,ib,ia,it))';
                else
                end
                % Lifetime production (survival-discounted ~ age structure of food contributions
                PT_matHH_Kb(iphi,itheta,ib,ia) = sum(Px_mat);
            end
        end
    end   
end
save PxHH_Kb PT_matHH_Kb -v7.3
% clear PT_matHH_Kb Kx_mat

%% Optimal teacher age (*CONSIDERING* teacher production effects)

% with only teacher age effects on skills ontogeny
load PxLL_Kb PT_matLL_Kb
isk = 1;
bopt_LL_Kb = zeros(length(phi),length(theta),length(a));
for iphi = 1:length(phi)-1
    for itheta = 2:length(theta)
        for ia = 1:length(a)
            if max(PT_matLL_Kb(iphi,itheta,ia:bmax,ia)) > PT_SK(isk)
                bmin_LL_Kb(iphi,itheta,ia) = min(find(PT_matLL_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bmax_LL_Kb(iphi,itheta,ia) = max(find(PT_matLL_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bopt_LL_Kb(iphi,itheta,ia) = find(PT_matLL_Kb(iphi,itheta,ia:bmax,ia)==max(PT_matLL_Kb(iphi,itheta,ia:bmax,ia)))+ia;
            else
                bmin_LL_Kb(iphi,itheta,ia) = NaN;
                bmax_LL_Kb(iphi,itheta,ia) = NaN;
                bopt_LL_Kb(iphi,itheta,ia) =  NaN;
            end
        end
    end
end

load PxHL_Kb PT_matHL_Kb
isk = 2;
bopt_HL_Kb = zeros(length(phi),length(theta),length(a));
for iphi = 1:length(phi)-1
    for itheta = 2:length(theta)
        for ia = 1:length(a)
            if max(PT_matHL_Kb(iphi,itheta,ia:bmax,ia)) > PT_SK(isk)
                bmin_HL_Kb(iphi,itheta,ia) = min(find(PT_matHL_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bmax_HL_Kb(iphi,itheta,ia) = max(find(PT_matHL_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bopt_HL_Kb(iphi,itheta,ia) = find(PT_matHL_Kb(iphi,itheta,ia:bmax,ia)==max(PT_matHL_Kb(iphi,itheta,ia:bmax,ia)))+ia;
            else
                bmin_HL_Kb(iphi,itheta,ia) = NaN;
                bmax_HL_Kb(iphi,itheta,ia) = NaN;
                bopt_HL_Kb(iphi,itheta,ia) =  NaN;
            end
        end
    end
end

load PxLH_Kb PT_matLH_Kb
isk = 3;
bopt_LH_Kb = zeros(length(phi),length(theta),length(a));
for iphi = 1:length(phi)-1
    for itheta = 2:length(theta)
        for ia = 1:length(a)
            if max(PT_matLH_Kb(iphi,itheta,ia:bmax,ia)) > PT_SK(isk)
                bmin_LH_Kb(iphi,itheta,ia) = min(find(PT_matLH_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bmax_LH_Kb(iphi,itheta,ia) = max(find(PT_matLH_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bopt_LH_Kb(iphi,itheta,ia) = find(PT_matLH_Kb(iphi,itheta,ia:bmax,ia)==max(PT_matLH_Kb(iphi,itheta,ia:bmax,ia)))+ia;
            else
                bmin_LH_Kb(iphi,itheta,ia) = NaN;
                bmax_LH_Kb(iphi,itheta,ia) = NaN;
                bopt_LH_Kb(iphi,itheta,ia) =  NaN;
            end
        end
    end
end

load PxHH_Kb PT_matHH_Kb
isk = 4;
bopt_HH_Kb = zeros(length(phi),length(theta),length(a));
for iphi = 1:length(phi)-1
    for itheta = 2:length(theta)
        for ia = 1:length(a)
            if max(PT_matHH_Kb(iphi,itheta,ia:bmax,ia)) > PT_SK(isk)
                bmin_HH_Kb(iphi,itheta,ia) = min(find(PT_matHH_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bmax_HH_Kb(iphi,itheta,ia) = max(find(PT_matHH_Kb(iphi,itheta,ia:bmax,ia)))+ia;
                bopt_HH_Kb(iphi,itheta,ia) = find(PT_matHH_Kb(iphi,itheta,ia:bmax,ia)==max(PT_matHH_Kb(iphi,itheta,ia:bmax,ia)))+ia;
            else
                bmin_HH_Kb(iphi,itheta,ia) = NaN;
                bmax_HH_Kb(iphi,itheta,ia) = NaN;
                bopt_HH_Kb(iphi,itheta,ia) =  NaN;
            end
        end
    end
end

save Px_opt_Kb