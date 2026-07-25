% This is the RST_plot_error() function which uses the data file to draw the sparse
% grid estimated errors.

% Input parameters:
% soludoc: the result document
% DIM: the number of dimensions

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_error( soludoc, DIM )
    soludoc = 'case2';
    DIM = 3;
    for i = 1 : DIM
        fh = figure();
        h = title(['Error of x',num2str(i)]);
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = xlabel('Point');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = ylabel('Error');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        hold on;
        temp = load([soludoc,'/error/x',num2str(i),'.txt']);
        for j = 1 : length(temp)
            plot(j,temp(j),'.');
        end
        hold off;
        saveas(fh, [soludoc,'/error/x',num2str(i),'.fig']);
    end
    
    for i = 1 : DIM
        fh = figure();
        h = title(['Error of y',num2str(i)]);
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = xlabel('Point');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = ylabel('Error');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        hold on;
        temp = load([soludoc,'/error/y',num2str(i),'.txt']);
        for j = 1 : length(temp)
            plot(j,temp(j),'.');
        end
        hold off;
        saveas(fh, [soludoc,'/error/y',num2str(i),'.fig']);
    end
    
    fh = figure();
    h = title('Error of xiL');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Point');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Error');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/error/xiL.txt']);
    for j = 1 : length(temp)
        plot(j,temp(j),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/error/xiL.fig']);
    
    fh = figure();
    h = title('Error of xiG');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Point');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Error');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/error/xiG.txt']);
    for j = 1 : length(temp)
        plot(j,temp(j),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/error/xiG.fig']);
    
    fh = figure();
    h = title('Error of rhoL');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Point');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Error');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/error/rhoL.txt']);
    for j = 1 : length(temp)
        plot(j,temp(j),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/error/rhoL.fig']);
    
    fh = figure();
    h = title('Error of rhoG');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Point');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Error');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/error/rhoG.txt']);
    for j = 1 : length(temp)
        plot(j,temp(j),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/error/rhoG.fig']);
    
    fh = figure();
    h = title('Error of sL');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Point');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Error');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/error/sL.txt']);
    for j = 1 : length(temp)
        plot(j,temp(j),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/error/sL.fig']);
    
    for i = 1 : DIM
        fh = figure();
        h = title(['Error of v',num2str(i)]);
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = xlabel('Point');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
        h = ylabel('Error');
        set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
        hold on;
        temp = load([soludoc,'/error/v',num2str(i),'.txt']);
        for j = 1 : length(temp)
            plot(j,temp(j),'.');
        end
        hold off;
        saveas(fh, [soludoc,'/error/v',num2str(i),'.fig']);
    end
    
    fh = figure();
    h = title('Error of Cf');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Point');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Error');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/error/Cf.txt']);
    for j = 1 : length(temp)
        plot(j,temp(j),'.');
    end
    hold off;
    saveas(fh, [soludoc,'/error/Cf.fig']);
    
end
