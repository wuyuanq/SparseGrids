% This is the RST_plot_3D() function which uses the data file to draw
% the 3D sparse grids.

% Input parameters:
% soludoc: the result document

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_3D()

    soludoc = 'case4';

    fh = figure();
    h = title('3D Union Sparse Grid');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('x1');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('x2');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    h = zlabel('x3');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    axis([0 1 0 1 0 1]);
    hold on;
    temp = load([soludoc, '/sg/union.txt']);
    for i = 1 : 3 : length(temp)
        plot3(temp(i),temp(i+1),temp(i+2),'.');
    end
    hold off;
    saveas(fh, [soludoc, '/sg/union.fig']);
    
end
