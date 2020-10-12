import traitlets

import ipyvuetify as v

def set_msg(pts, bands_combo, source_name, basename):
    
    nb_pts = len(pts)
    
    #compute the surface 
    pts_conform = pts.to_crs('ESRI:54009')
    minx, miny, maxx, maxy = pts_conform.total_bounds
    surface = (maxx-minx)*(maxy-miny)/10e6 #in km2
    
    msg = """
        <div>
            <p>
                You're about to launch the following downloading :
            <p>
            <ul>
                <li>
                    <b>{}</b> points distributed on <b>{:.2f}</b> km\u00B2
                </li>
                <li>
                    Using the images coming from <b>{}</b> satellites
                <li>
                    Using the <b>{}</b> band combination
                </li>
                <li>
                    Saved in a file using <b>{}</b> as a basename
                </li>
            </ul>
            
            <p>
                If you agree with these input you can start the downloading, if not please change the inputs in the previous tiles
            </p>
        </div>
    """.format(nb_pts, surface, source_name, bands_combo, basename)
    
    #create a Html widget
    class MyHTML(v.VuetifyTemplate):
        template = traitlets.Unicode(msg).tag(sync=True)
    
    
    return MyHTML()
    
    
    
    
    