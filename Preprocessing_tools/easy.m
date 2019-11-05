function varargout = easy(varargin)
%
%==================================================================== 
%  EASY is a GUI for the easy Grid package
%  execute by typing: easy
%
%  (c) 2008, Jeroen Molemaker
%      2019, P. Marchesiello, modified for croco_tools
%                             (called by make_grid)
%==================================================================== 
%
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @easy_OpeningFcn, ...
                   'gui_OutputFcn',  @easy_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end

% End initialization code - DO NOT EDIT

%======================================================================

function easy_OpeningFcn(hObject, eventdata, handles, varargin)

%
% --- Open Easy window and set grid parameters from crocotools_param.h
%     or from a *mat file previously created by Easy
%

str = evalc('crocotools_param');
R_earth=6367442.76;   % Earth radius
deg2rad=pi/180;
rad2deg=180/pi;
%
% Choose default command line output for easy
%
handles.output = hObject;
%
% Update handles structure
%
guidata(hObject, handles);
%
% This sets up the initial plot - only do when we are invisible
% so window can get raised using easy.
%
if strcmp(get(hObject,'Visible'),'off')
 %
 % Initialize grid parameters from crocotools_param
 %
 rotate  = 0;
 tra_lon = (lonmin+lonmax)/2.;
 tra_lat = (latmin+latmax)/2.;
 size_x  = R_earth*cos(tra_lat*deg2rad)*(lonmax-lonmin)*deg2rad;
 size_y  = R_earth*(latmax-latmin)*deg2rad;
 if size_x>100.e3, size_x=1.e3*floor(size_x/1.e3); end
 if size_y>100.e3, size_y=1.e3*floor(size_y/1.e3); end
 dx = R_earth*dl*deg2rad; % nx,ny grid sizes
 dy = dx;                 % from grid resolution dl (deg)
 nx = floor(size_x/dx);
 ny = floor(size_y/dy);
 %
 % Recover parameters from files if available
 %
 if exist('easy_grid_params.mat')
  load('easy_grid_params')
 end
 %
 % Get metrics
 %
 [lon,lat,pm,pn,ang] = easy_grid(nx,ny,dl,size_x,size_y,tra_lon,tra_lat,rotate);
 %
 % Correct nx,ny if needed
 %
 [Mp Lp]=size(lon);
 nx=Lp-2;
 ny=Mp-2;
 %
 % Display grid parameters 
 %
 set(handles.edit1,'String',num2str(size_x/1.e3));  % size_x (km)
 set(handles.edit2,'String',num2str(size_y/1.e3));  % size_y (km)
 set(handles.edit3,'String',num2str(rotate));       % Rotation
 set(handles.edit4,'String',num2str(tra_lon));      % Lon Center
 set(handles.edit5,'String',num2str(tra_lat));      % Lat Center
 set(handles.edit6,'String',num2str(nx));           % nx
 set(handles.edit7,'String',num2str(ny));           % ny
 set(handles.edit10,'String',num2str(dl));          % Mesh size dl (deg)
 %
 % Plot domain grid outline
 %
 colormap(jet(256))
 radius = sqrt(size_x^2+size_y^2)/R_earth*rad2deg;
 dll  = 0.1*radius;
 lonmin0 = min(min(lon)) - dll;
 lonmax0 = max(max(lon)) + dll;
 latmin0 = min(min(lat)) - dll;
 latmax0 = max(max(lat)) + dll;
 if rotate==0 | radius>40,
  m_proj ('miller cylindrical','longitude',[lonmin0 lonmax0],'latitude',[latmin0 latmax0]);
 else
  m_proj('Gnomonic','lon',tra_lon,'lat',tra_lat,'rad',radius,'rec','on')
 end
 out_lon = [lon(1,:) lon(:,end)' lon(end,end:-1:1) lon(end:-1:1,1)'];
 out_lat = [lat(1,:) lat(:,end)' lat(end,end:-1:1) lat(end:-1:1,1)'];
 m_plot(out_lon,out_lat,'r')
 m_grid
 hold on
 if min(size_x,size_y)<100.e3
  m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
 else
  m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
 end
 hold off
end
clear

% --------------------------------------------------------------------
function varargout = easy_OutputFcn(hObject, eventdata, handles)

% Get default command line output from handles structure
varargout{1} = handles.output;

% --------------------------------------------------------------------
function pushbutton1_Callback(hObject, eventdata, handles)

% --- Executes "Update" on button press in pushbutton1.

set(handles.pushbutton1,'enable','off');
axes(handles.axes1);
cla;

str = evalc('crocotools_param');
R_earth=6367442.76;   % Earth radius
deg2rad=pi/180;
rad2deg=180/pi;

%
% Get updated grid parameters
%
popup_sel_index = get(handles.popupmenu1, 'Value');
size_x = str2double(get(handles.edit1,'String'))*1e3; %% Width 
size_y = str2double(get(handles.edit2,'String'))*1e3; 
rotate = str2double(get(handles.edit3,'String'));
if abs(rotate) > 45
  rotate = rotate/abs(rotate)*45;
  set(handles.edit3,'String',num2str( rotate));       %% Rotation
end
tra_lon= str2double(get(handles.edit4,'String'));
tra_lat= str2double(get(handles.edit5,'String'));
nx     = str2num(get(handles.edit6,'String'));
ny     = str2num(get(handles.edit7,'String')); 
dl     = str2num(get(handles.edit10,'String'));
%
% Update nx,ny from updated sizes
% 
dx=R_earth*dl*deg2rad;
dy=dx;
nx=floor(size_x/dx);
ny=floor(size_y/dy);
nx=max(nx,5);
ny=max(ny,5);
if nx==5, dl=(size_x/nx)/R_earth*rad2deg; end;
set(handles.edit6,'String',num2str(nx));           %% nx
set(handles.edit7,'String',num2str(ny));           %% ny
set(handles.edit10,'String',num2str(dl));          % Mesh size dl (deg)
%
% Get updated metrics
%
[lon,lat,pm,pn,ang] = easy_grid(nx,ny,dl,size_x,size_y,tra_lon,tra_lat,rotate);
%
% Correct nx,ny if needed
%
[Mp Lp]=size(lon);
nx=Lp-2;
ny=Mp-2;
%
% Save updated grid parameters
%
save('easy_grid_params', ...
     'nx','ny','dl','size_x','size_y','rotate','tra_lon','tra_lat')
%
% Set plot projection
%
colormap(jet(256))
radius = sqrt(size_x^2+size_y^2)/R_earth*rad2deg;
dll  = 0.1*radius;
lonmin0 = min(min(lon)) - dll;
lonmax0 = max(max(lon)) + dll;
latmin0 = min(min(lat)) - dll;
latmax0 = max(max(lat)) + dll;
if rotate==0 | radius>40,
 m_proj ('miller cylindrical','longitude',[lonmin0 lonmax0],'latitude',[latmin0 latmax0]);
else
 m_proj('Gnomonic','lon',tra_lon,'lat',tra_lat,'rad',radius,'rec','on')
end
out_lon = [lon(1,:) lon(:,end)' lon(end,end:-1:1) lon(end:-1:1,1)'];
out_lat = [lat(1,:) lat(:,end)' lat(end,end:-1:1) lat(end:-1:1,1)'];
%
%  Plot grid (outline, grid, topo, pn, pm, angle)
%
switch popup_sel_index
    case 1
      m_grid
      hold on
      if min(size_x,size_y)<100.e3
       m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
      else
       m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
      end
      m_plot(out_lon,out_lat,'r')
      hold off

    case 2
      m_grid
      hold on
      if min(size_x,size_y)<100.e3
       m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
      else
       m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
      end
      m_plot(lon,lat,'.b')
      hold off

    case 3
      nc1 = netcdf(topofile); 
      x = nc1{'lon'}(:);
      y = nc1{'lat'}(:);
      d = nc1{'topo'}(:);
      close(nc1);
      x1=x(find(x<0));
      x2=x(find(x>=0));
      d1=d(:,find(x<0));
      d2=d(:,find(x>=0));
      x1 = x1 +360;
      x = [x2' x1']';
      d = [d2 d1];
      xm = x-360;
      x = [xm' x']';
      d = [d d];
      di = interp2(x,y,d,lon,lat);
      di(di>10) = 10.;
      m_pcolor(lon,lat,di);shading flat;colorbar
      m_grid
      hold on
      if min(size_x,size_y)<100.e3
       m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
      else
       m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
      end
      m_plot(out_lon,out_lat,'r')
      hold off

    case 4
      m_pcolor(lon,lat,pn);shading flat;colorbar
      m_grid
      if min(size_x,size_y)<100.e3
       m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
      else
       m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
      end

    case 5
      m_pcolor(lon,lat,pm);shading flat;colorbar
      m_grid
      if min(size_x,size_y)<100.e3
       m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
      else
       m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
      end

    case 6
      m_pcolor(lon,lat,ang);shading flat;colorbar
      m_grid
      if min(size_x,size_y)<100.e3
       m_gshhs_f('patch',[.7 .7 .7],'edgecolor','k')
      else
       m_gshhs_i('patch',[.7 .7 .7],'edgecolor','k')
      end
end

set(handles.pushbutton1,'enable','on');

% --------------------------------------------------------------------
function pushbutton4_Callback(hObject, eventdata, handles)

% --- Executes "Apply" on button press in pushbutton4.

bgClr = get(handles.pushbutton4,'BackgroundColor');
set(handles.pushbutton4,'BackgroundColor',[0 0 1]);

str = evalc('crocotools_param');
R_earth=6367442.76;   % Earth radius
deg2rad=pi/180;
rad2deg=180/pi;
%
% Get metrics
%
size_x = str2double(get(handles.edit1,'String'))*1e3; %% Width 
size_y = str2double(get(handles.edit2,'String'))*1e3; 
rotate = str2double(get(handles.edit3,'String'));
if abs(rotate) > 45
  rotate = rotate/abs(rotate)*45;
  set(handles.edit3,'String',num2str( rotate));       %% Rotation
end
tra_lon= str2double(get(handles.edit4,'String'));
tra_lat= str2double(get(handles.edit5,'String'));
nx     = str2num(get(handles.edit6,   'String'));
ny     = str2num(get(handles.edit7,   'String')); 
dl     = str2num(get(handles.edit10,  'String'));
%
% Update nx,ny from updated sizes 
% (security in case "Update" stage skipped)
% 
dx=R_earth*dl*deg2rad;
dy=dx;
nx=floor(size_x/dx);
ny=floor(size_y/dy);
nx=max(nx,5);
ny=max(ny,5);
if nx==5, dl=(size_x/nx)/R_earth*rad2deg; end;
set(handles.edit6, 'String',num2str(nx));           %% nx
set(handles.edit7, 'String',num2str(ny));           %% ny
set(handles.edit10,'String',num2str(dl));          % Mesh size dl (deg)
%
[lon,lat,pm,pn,ang] = easy_grid(nx,ny,dl,size_x,size_y,tra_lon,tra_lat,rotate);
%
% Correct nx,ny if needed
%
[Mp Lp]=size(lon);
nx=Lp-2;
ny=Mp-2;
%
% Make grid file and fill lat,lon fields
%
create_grid(nx+1,ny+1,grdname,CROCO_title) % create grid file
%
lon_rho = lon; % degrees
lat_rho = lat;
[lon_u,lon_v,lon_p]=rho2uvp(lon_rho);
[lat_u,lat_v,lat_p]=rho2uvp(lat_rho);
nc=netcdf(grdname,'write');
nc{'lon_rho'}(:)  = lon_rho;  
nc{'lat_rho'}(:)  = lat_rho;
nc{'lon_u'}(:)    = lon_u;
nc{'lat_u'}(:)    = lat_u;
nc{'lon_v'}(:)    = lon_v;
nc{'lat_v'}(:)    = lat_v;
nc{'lon_psi'}(:)  = lon_p;
nc{'lat_psi'}(:)  = lat_p;
close(nc);

pause(0.1)
set(handles.pushbutton4,'BackgroundColor',bgClr);

close(easy)

% --------------------------------------------------------------------
function FileMenu_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function OpenMenuItem_Callback(hObject, eventdata, handles)

file = uigetfile('*.fig');
if ~isequal(file, 0)
    open(file);
end

% --------------------------------------------------------------------
function PrintMenuItem_Callback(hObject, eventdata, handles)

printdlg(handles.figure1)

% --------------------------------------------------------------------
function CloseMenuItem_Callback(hObject, eventdata, handles)

selection = questdlg(['Close ' get(handles.figure1,'Name') '?'],...
                     ['Close ' get(handles.figure1,'Name') '...'],...
                     'Yes','No','Yes');
if strcmp(selection,'No')
    return;
end

delete(handles.figure1)

% --------------------------------------------------------------------
function popupmenu1_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function popupmenu1_CreateFcn(hObject, eventdata, handles)

if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

set(hObject, 'String', {'outline', 'grid', 'topo', 'pn', 'pm', 'angle'});

% --------------------------------------------------------------------
function edit1_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit1_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit2_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit2_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit3_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit3_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit4_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit4_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit5_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit5_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit6_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit6_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit7_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit7_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function edit10_Callback(hObject, eventdata, handles)

% --------------------------------------------------------------------
function edit10_CreateFcn(hObject, eventdata, handles)

if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


