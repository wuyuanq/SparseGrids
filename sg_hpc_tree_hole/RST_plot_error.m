% This is the RST_plot_error() function which uses the data file to draw the sparse
% grid estimated errors.

% Input parameters:
% soludoc: the result document
% DIM: the number of dimensions

% Author: Yuanqing Wu. Email: wuyuanq@gmail.com
% Last edited on October 23rd, 2017

function RST_plot_error( soludoc, DIM )

    for i = 1 : DIM
%         fh = figure();
%         h = title(['Error of x',num2str(i)]);
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%         h = xlabel('Point');
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%         h = ylabel('Error');
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%         hold on;
        temp = load([soludoc,'/error/x',num2str(i),'.txt']);
        disp(['The average error of x',num2str(i),' is ',num2str(mean(temp))]); 
%         for j = 1 : length(temp)
%             plot(j,temp(j),'.');
%         end
%         hold off;
%         saveas(fh, [soludoc,'/error/x',num2str(i),'.fig']);
    end
%     
    for i = 1 : DIM
%         fh = figure();
%         h = title(['Error of y',num2str(i)]);
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%         h = xlabel('Point');
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%         h = ylabel('Error');
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%         hold on;
        temp = load([soludoc,'/error/y',num2str(i),'.txt']);
        disp(['The average error of y',num2str(i),' is ',num2str(mean(temp))]); 
%         for j = 1 : length(temp)
%             plot(j,temp(j),'.');
%         end
%         hold off;
%         saveas(fh, [soludoc,'/error/y',num2str(i),'.fig']);
    end
%     
%     fh = figure();
%     h = title('Error of xiL');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = xlabel('Point');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = ylabel('Error');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%     hold on;
    temp = load([soludoc,'/error/xiL.txt']);
    disp(['The average error of xiL is ',num2str(mean(temp))]); 
%     for j = 1 : length(temp)
%         plot(j,temp(j),'.');
%     end
%     hold off;
%     saveas(fh, [soludoc,'/error/xiL.fig']);
%     
%     fh = figure();
%     h = title('Error of xiG');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = xlabel('Point');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = ylabel('Error');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%     hold on;
    temp = load([soludoc,'/error/xiG.txt']);
    disp(['The average error of xiG is ',num2str(mean(temp))]); 
%     for j = 1 : length(temp)
%         plot(j,temp(j),'.');
%     end
%     hold off;
%     saveas(fh, [soludoc,'/error/xiG.fig']);
%     
%     fh = figure();
%     h = title('Error of rhoL');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = xlabel('Point');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = ylabel('Error');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%     hold on;
    temp = load([soludoc,'/error/rhoL.txt']);
    disp(['The average error of rhoL is ',num2str(mean(temp))]); 
%     for j = 1 : length(temp)
%         plot(j,temp(j),'.');
%     end
%     hold off;
%     saveas(fh, [soludoc,'/error/rhoL.fig']);
%     
%     fh = figure();
%     h = title('Error of rhoG');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = xlabel('Point');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = ylabel('Error');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%     hold on;
    temp = load([soludoc,'/error/rhoG.txt']);
    disp(['The average error of rhoG is ',num2str(mean(temp))]); 
%     for j = 1 : length(temp)
%         plot(j,temp(j),'.');
%     end
%     hold off;
%     saveas(fh, [soludoc,'/error/rhoG.fig']);
%     
%     fh = figure();
%     h = title('Error of sL');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = xlabel('Point');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = ylabel('Error');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%     hold on;
    temp = load([soludoc,'/error/sL.txt']);
    disp(['The average error of sL is ',num2str(mean(temp))]); 
%     for j = 1 : length(temp)
%         plot(j,temp(j),'.');
%     end
%     hold off;
%     saveas(fh, [soludoc,'/error/sL.fig']);
%     
    for i = 1 : DIM
%         fh = figure();
%         h = title(['Error of v',num2str(i)]);
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%         h = xlabel('Point');
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%         h = ylabel('Error');
%         set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%         hold on;
        temp = load([soludoc,'/error/v',num2str(i),'.txt']);
        disp(['The average error of v',num2str(i),' is ',num2str(mean(temp))]); 
%         for j = 1 : length(temp)
%             plot(j,temp(j),'.');
%         end
%         hold off;
%         saveas(fh, [soludoc,'/error/v',num2str(i),'.fig']);
    end
%     
%     fh = figure();
%     h = title('Error of Cf');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = xlabel('Point');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
%     h = ylabel('Error');
%     set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
%     hold on;
    temp = load([soludoc,'/error/Cf.txt']);
    disp(['The average error of Cf is ',num2str(mean(temp))]); 
%     for j = 1 : length(temp)
%         plot(j,temp(j),'.');
%     end
%     hold off;
%     saveas(fh, [soludoc,'/error/Cf.fig']);
    
end
