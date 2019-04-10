Shiny.addCustomMessageHandler('create_ceo_project',
    function(message) {
        const { classes, title, plotSize, csv } = message;
        const rows = csv.split('\n');
        rows.shift(); // remove first element
        if (rows[rows.length - 1] === "") rows.pop(); // remove last element if empty
        const plots = rows.map((row) => {
            const values = row.split(',');
            const [ , y, x] = values;
            return {
                lat: y.replace(/"/g, ''),
                lon: x.replace(/"/g, '')
            };
        });
        const json = {
            classes,
            title,
            plotSize: parseInt(plotSize),
            plots
        };
        const xmlHttp = new XMLHttpRequest();
        xmlHttp.open('POST', '/api/ceo-gateway/create-project', false); // false for synchronous request
        xmlHttp.setRequestHeader('Content-Type', 'application/json');
        xmlHttp.send(JSON.stringify(json));
        const {status, responseText} = xmlHttp;
        let res = {
          status: xmlHttp.status,
          responseText: xmlHttp.responseText
        };
        if (status === 200) {
            const id = responseText.split('/').slice(-1)[0];
            res = {
                ...res,
                id,
                title
            };
        }
        Shiny.onInputChange('jsEvent', res);
    }
);