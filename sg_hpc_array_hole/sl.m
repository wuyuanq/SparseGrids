
nx = 500;
ny = 500;

% load data
slf = zeros(ny, nx);
temp = load('case2/slf.txt');
k = 1;
for j = 1 : nx %p
    for i = 1 : ny %z
        slf(i,j) = temp(k);
        k = k + 1;
    end
end

sls = zeros(ny, nx);
temp = load('case2/sls.txt');
k = 1;
for j = 1 : nx
    for i = 1 : ny
        sls(i,j) = temp(k);
        k = k + 1;
    end
end   

% construct a grid
xx = zeros(nx, 1);
for i = 1 : nx
    xx(i) = 3000000.0+1000000.0*i/(nx+1); 
end
yy = zeros(ny, 1);
for i = 1 : ny
    yy(i) = i/(ny+1)*1.0; 
end

% begin to draw the images
fh = figure();
h = title('Saturation of the oil phase from flash calculations');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = xlabel('Pressure');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = ylabel('Molar fraction of methane');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
hold on;
[X,Y] = meshgrid(xx, yy); 
contourf(X, Y, slf, 200, 'linecolor', 'none');
t = colorbar;
set(get(t,'title'), 'string', 'saturation', 'Fontsize', 12);
hold on;
for j = 1 : nx %p
    for i = 1 : ny %z
        if(abs(slf(i,j))<1.e-7)
           plot(xx(j),yy(i),'g.','MarkerSize',10); 
           break;
        end
    end
end
hold on;
for j = 1 : nx %p
    for i = ny : -1 : 1 %z
        if(abs(slf(i,j)-1.0)<1.e-7)
           plot(xx(j),yy(i),'k.','MarkerSize',10); 
           break;
        end
    end
end
hold off;
saveas(fh, 'Figure1.fig');

fh = figure();
h = title('Saturation of the oil phase from the sparse grid interpolation');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = xlabel('Pressure');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'HorizontalAlignment', 'center');
h = ylabel('Molar fraction of methane');
set(h, 'fontsize', 16, 'FontWeight', 'bold', 'VerticalAlignment', 'middle');
hold on;
contourf(X, Y, sls, 200, 'linecolor', 'none');
t = colorbar;
set(get(t,'title'), 'string', 'saturation', 'Fontsize', 12);
for j = 1 : nx %p
    for i = 1 : ny %z
        if((abs(sls(i,j))<1.e-7)||(sls(i,j)<=0.0))
        %if(abs(sls(i,j))<1.e-7)
           plot(xx(j),yy(i),'g.','MarkerSize',10); 
           break;
        end
    end
end
hold on;
for j = 1 : nx %p
    for i = ny : -1 : 1 %z
        if((sls(i,j)>=1.0)||(abs(slf(i,j)-1.0)<1.e-7))
        %if(abs(sls(i,j)-1.0)<1.e-7)
           plot(xx(j),yy(i),'k.','MarkerSize',10); 
           break;
        end
    end
end
hold off;
saveas(fh, 'Figure2.fig');
