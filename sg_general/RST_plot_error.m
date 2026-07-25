% This is the RST_plot_error() function which uses the data file to draw the sparse
% grid estimated errors.

% Input parameters:
% soludoc: the result document
% DIM: the number of dimensions

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_error()

    soludoc = 'case4';
    SURPLUSSIZE = 4;
    
    for i = 1 : SURPLUSSIZE
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
    
end
