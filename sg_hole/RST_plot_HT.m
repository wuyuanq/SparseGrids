% This is the RST_plot_HT() function which uses the data file to draw the sparse
% grid hash table.

% Input parameters:
% soludoc: the result document

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_HT( soludoc )

    fh = figure();
    h = title('Hash Table Size');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = xlabel('Node');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
    h = ylabel('Size');
    set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
    hold on;
    temp = load([soludoc,'/numHash.txt']);
    for i = 1:length(temp)
        plot(i,temp(i),'.');
    end
    hold off;
    saveas(fh, [soludoc, '/numHash.fig']);

end
