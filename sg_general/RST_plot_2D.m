% This is the RST_plot_2D() function which uses the data file to draw the 2D
% sparse grids.

% Input parameters:
% soludoc: the result document

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_2D()

    soludoc = 'case4';

    fh = figure();
    h = title('2D United Sparse Grid');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('x1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('x2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1]);
    hold on;
    
    temp = load([soludoc, '/sg/union.txt']);
    for i = 1:2:length(temp)
        plot(temp(i),temp(i+1),'.');
    end
    hold off;
    saveas(fh, [soludoc, '/sg/union.fig']);

end
