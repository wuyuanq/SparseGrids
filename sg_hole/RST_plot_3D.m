% This is the RST_plot_3D() function which uses the data file to draw
% the 3D sparse grids.

% Input parameters:
% soludoc: the result document

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_3D( soludoc )

    fh = figure();
    h = title('3D Union Sparse Grid');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/union.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/union.fig']);
    
    for i = 1 : 3
        fh = figure();
        h = title(['3D Sparse Grid of x',num2str(i)]);
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = xlabel('P');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = ylabel('z1');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        h = zlabel('z2');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        axis([0 1 0 1 0 1]);
        hold on;
        temp = load([soludoc,'/sg/x',num2str(i),'.txt']);
        for j = 1:3:length(temp)
            plot3(temp(j),temp(j+1),temp(j+2),'.');
        end
        hold off;
        saveas(fh, [soludoc,'/sg/x',num2str(i),'.fig']); 
    end
    
    for i = 1 : 3
        fh = figure();
        h = title(['3D Sparse Grid of y',num2str(i)]);
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = xlabel('P');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = ylabel('z1');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        h = zlabel('z2');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        axis([0 1 0 1 0 1]);
        hold on;
        temp = load([soludoc,'/sg/y',num2str(i),'.txt']);
        for j = 1:3:length(temp)
            plot3(temp(j),temp(j+1),temp(j+2),'.');
        end
        hold off;
        saveas(fh, [soludoc,'/sg/y',num2str(i),'.fig']); 
    end
    
    fh = figure();
    h = title('3D Sparse Grid of xiL');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/xiL.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/xiL.fig']);
    
    fh = figure();
    h = title('3D Sparse Grid of xiG');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/xiG.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/xiG.fig']);
    
    fh = figure();
    h = title('3D Sparse Grid of rhoL');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/rhoL.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/rhoL.fig']);
    
    fh = figure();
    h = title('3D Sparse Grid of rhoG');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/rhoG.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/rhoG.fig']);
    
    fh = figure();
    h = title('3D Sparse Grid of sL');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/sL.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/sL.fig']);
    
    for i = 1 : 3
        fh = figure();
        h = title(['3D Sparse Grid of v',num2str(i)]);
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = xlabel('P');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = ylabel('z1');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        h = zlabel('z2');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        axis([0 1 0 1 0 1]);
        hold on;
        temp = load([soludoc,'/sg/v',num2str(i),'.txt']);
        for j = 1:3:length(temp)
            plot3(temp(j),temp(j+1),temp(j+2),'.');
        end
        hold off;
        saveas(fh, [soludoc,'/sg/v',num2str(i),'.fig']); 
    end
     
	fh = figure();
    h = title('3D Sparse Grid of Cf');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('P');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('z1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('z2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc,'/sg/Cf.txt']);
    for i = 1:3:length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/sg/Cf.fig']);
    
end
