function tf = mexnc_use_tmw()

switch ( version('-release') )
    case { '11', '12', '13', '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
		tf = false;
	otherwise
		tf = true;
end

