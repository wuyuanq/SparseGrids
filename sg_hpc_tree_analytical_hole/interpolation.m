
path('/Users/yuanqingwu/research/sg_hpc_tree', path);
fh = figure();
strtitle = 'Average interpolation time';
h = title(strtitle);
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = xlabel('Maximal layer');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = ylabel('Time');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
hold on;

layer = 7:11; 
treet = zeros(5, 1);
treet(1) = 3.490e-4;
treet(2) = 1.243e-3;
treet(3) = 2.361e-3;
treet(4) = 4.860e-3;
treet(5) = 9.369e-3;
plot(layer, treet);
hold on;
arrayt = zeros(5, 1);
arrayt(1) = 3.657e-6;
arrayt(2) = 1.412e-5;
arrayt(3) = 6.160e-5;
arrayt(4) = 1.445e-4;
arrayt(5) = 3.883e-4;
plot(layer, arrayt);
hold on;
set(gca,'XTick',7:11)
set(gca,'XTickLabel',{'7','8','9','10','11'})
legend('tree','array','location','northwest')
hold on;

saveas(fh, 'interpolation.fig');
rmpath('/Users/yuanqingwu/research/sg_hpc_tree');
