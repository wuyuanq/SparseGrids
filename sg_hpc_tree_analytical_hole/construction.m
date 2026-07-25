
path('/Users/yuanqingwu/research/sg_hpc_tree', path);
fh = figure();
strtitle = 'Construction time';
h = title(strtitle);
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = xlabel('Maximal layer');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = ylabel('Time');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
hold on;

layer = 7:11; 
treet = zeros(5, 1);
treet(1) = 0.316;
treet(2) = 1.379;
treet(3) = 3.795;
treet(4) = 16.946;
treet(5) = 58.536;
plot(layer, treet);
hold on;
arrayt = zeros(5, 1);
arrayt(1) = 3.232;
arrayt(2) = 28.860;
arrayt(3) = 271.061;
arrayt(4) = 6013.389;
arrayt(5) = 94966.363;
plot(layer, arrayt);
hold on;
set(gca,'XTick',7:11)
set(gca,'XTickLabel',{'7','8','9','10','11'})
legend('tree','array','location','northwest')
hold on;

saveas(fh, 'construction.fig');
rmpath('/Users/yuanqingwu/research/sg_hpc_tree');
