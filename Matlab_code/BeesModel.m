function ModeloAbejas
clc
    beta=0.92;omega=0.95;rho=0.23;mb=0.1; %parameter values 
    mt=0.29;a=0.018;u=1;v=1;delta=0.5;
    R1=0.26;R2=0.5;s2=0.82;s1=0.3;s0=0.07;
    
    x0=1;z0=1; %initial conditios 
    
    limaxis=1; %limit of axis
    Ti = 0; % intial time 
    Tf = 100; %end time
    y0_Vector=[0.15, 0.35, 0.7]; % different initial values for the population of adult bees
    
    for j=1:length(y0_Vector)
        y0=y0_Vector(j);
    
    [X,Z]=meshgrid(0:0.05:limaxis,0:0.05:limaxis);
    Y1=R1+0*X;
    Y2=R2+0*X;
    
    surf(X,Y1,Z);hold on;
    surf(X,Y2,Z);hold on; 
    figure(1);
 
    options = odeset('RelTol',1e-4,'AbsTol',[1e-4,1e-4,1e-4]);
    [t,S] = ode45(@SistemaEdo,[Ti,Tf],[x0,y0,z0],options); % solves the system of equations
    figure(2),
    set(0,'DefaultTextInterpreter', 'latex')
       
    if j==1
            col='r';
        end
        if j==2
            col='b';
        end
        if j==3
            col='g';
      
    end
    
    figure(2);
    subplot(131);plot(t,S(:,1),'LineWidth',2.5,'Color',col); hold on; xlabel('Time in weeks', 'Fontsize',14),ylabel('Population of immature bees','Fontsize',16)
    grid on
    subplot(132);plot(t,S(:,2),'LineWidth',2.5,'Color',col); hold on; xlabel('Time in weeks', 'Fontsize',14),ylabel('Population of adult bees','Fontsize',16)
    grid on
    subplot(133);plot(t,S(:,3),'LineWidth',2.5,'Color',col); hold on; xlabel('Time in weeks', 'Fontsize',14),ylabel('Amount of honey','Fontsize',16)
    grid on
    
    figure(1);
    plot3(S(:,1),S(:,2),S(:,3),'LineWidth',3,'Color',col); hold on; xlabel('Population of immature bees', 'Fontsize',14),ylabel('Population of adult bees','Fontsize',14);zlabel('Amount of honey','Fontsize',14)
    xlim([0 limaxis]);zlim([0 limaxis]);ylim([0 limaxis]);
    alpha(.6);
    
        
    end
   
                
    function dXdt = SistemaEdo(t,A)   % this function defines the dynamic system
        dXdt=zeros(3,1);
        dXdt(1) = beta*(A(2)/(A(2)+v))-omega*A(1)-mb*A(1);
        dXdt(2) = omega*A(1)-mt*A(2)-A(2)*E(A(2));
        dXdt(3) = rho*(A(2)/(A(2)+u))-a*A(3)-delta*A(2)*A(3);
    end
   
    function E=E(n)
        if n>R2
            E=s2;
        elseif n>=R1 && n<=R2
            E=s1;
        else
            E=s0;
        end
    end
end