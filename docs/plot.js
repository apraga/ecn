//Many thanks to https://www.d3-graph-gallery.com/graph/connectedscatter_multi.html
//
function formatDate(data) {
    data.forEach(function(x) {
        x.annee = new Date(+x.annee, 0);
        x.rang = +x.rang;
    })
}

// Every liht solazride except backgroound
function solarizedPalette(towns) {
    return d3.scaleOrdinal(
        [
            "#002b36" // base03
            // #073642 // base02
            , "#586e75" // base01
            , "#657b83" // base00
            , "#839496" // base0
            , "#93a1a1" // base1
            // #eee8d5 // base2
            // #fdf6e3 // base3
            , "#b58900" // yellow
            , "#cb4b16" // orange
            , "#d30102" // red
            , "#d33682" // magenta
            , "#6c71c4" // violet
            , "#268bd2" // blue
            , "#2aa198" // cyan
            , "#859900" // green
        ]).domain(towns);
}

function setYAxis(height) {
    return d3.scaleLinear()
        .domain([0, 8900]) // TODO
        .range([0, height]);
}

function setXAxis(width) {
    return  d3.scaleTime()
        .domain([new Date(2016, 0), new Date(2020, 0)])
        .range([ 0, width ]);
}
function createCheckboxes(svg, towns, myColor) {
    // Generate checkbox
    d3.select("body").selectAll("input")
        .data(towns)
        .enter()
        .append('label')
        .attr('for',function(d,i){ return 'a'+i; })
        .text(function(d) { return d; })
        .style("color", function(d){ return myColor(d[0]) })
        .append("input")
        .property("checked", false)
        .attr("type", "checkbox")
        .attr("id", function(d,i) { return 'a'+i; })
        .on("click", function(d){
            // This.__data__ is a bit ugly to get the name but it works
            var p = svg.select("path[class=" + this.__data__ + "]").transition();
            p.style("opacity", this.checked == 1 ? 1 : 0.1)
            var c = svg.select("g[class=" + this.__data__ + "]").selectAll("circle").transition();
            c.style("opacity", this.checked == 1 ? 1 : 0.1)
        })
    // Only show first value: set the
    svg.selectAll("path[class="+towns[0]+"]").style("opacity", 1); // Set path
    svg.selectAll("g[class="+towns[0]+"]").selectAll("circle").style("opacity", 1); // Set circles
    d3.select("input[id=a0]").property("checked", true) // Checkbox
}


function plotSpe(speTitle, rankmax, svg, width, height) {
    // Speciality is no longer a variable
    spe = rankMax.get(speTitle);
    // // Get the list of town for categories
    towns = Array.from(spe.keys());

    // Date  = X axis
    var x = setXAxis(width);
    svg.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x).ticks(d3.timeYear.every(1)));

    // Rank = Y axis
    var y = setYAxis(height);
    svg.append("g").call(d3.axisLeft(y));

    var myColor = solarizedPalette(towns);
    // .range(d3.schemePaired);
    // New strategy : town = category, x = yeary, y = rank
    // Draw line
    // Add the lines
    var line = d3.line()
        .x(function(d) { return x(d[0], 0) })
        .y(function(d) { return y(d[1]) })
    svg.selectAll("myLines")
        .data(spe)
        .enter()
        .append("path")
        .attr("class", function(d){ return d[0] })
        .attr("d", function(d){ return line(d[1]) } )
        .attr("stroke", function(d){return myColor(d[0]); })
        .style("stroke-width", 4)
        .style("fill", "none")
        .style("opacity", "0.1")

    svg.selectAll("myDots")
        .data(spe)
        .enter()
        .append('g')
        .attr("fill", function(d){ return myColor(d[0]) })
        .attr("class", function(d){ return d[0] })
    // Second we need to enter in the 'values' part of this group
        .selectAll("myPoints")
        .data(function(d){ return d[1] })
        .enter()
        .append("circle")
        .attr("cx", function(d) { return x(d[0]) } )
        .attr("cy", function(d) {return y(d[1]) } )
        .attr("r", 5)
        .attr("stroke", "white")
        .style("opacity", "0.1")

    // Interactive for new version ofD3
    // const tooltip = new Tooltip();

    createCheckboxes(svg, towns, myColor);
}

function createChoice(svg, rankMax, width, height){
    d3.select("body").selectAll("h2").html("Rang maximal pour <select id=\"selectSpe\"></select>");
    allSpe = Array.from(rankMax.keys());

    // add the options to the drow-down list
    d3.select("#selectSpe")
        .selectAll('myOptions')
        .data(allSpe)
        .enter()
        .append('option')
        .text(function (d) { return d; }) // text showed in the menu
        .attr("value", function (d) { return d; }) // corresponding value return

    // When the button is changed, run the updateChart function
    d3.select("#selectSpe").on("change", function(d) {
        // Don't forget to clean svg !!
        svg.selectAll('*').remove();
        var speTitle = d3.select(this).property("value")
        plotSpe(speTitle, rankMax, svg, width, height);
    })
}

function createSVG(width, height, margin) {
    return d3.select("#myplot")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");
}

function plot(data){

    // set the dimensions and margins of the graph
    var margin = {top: 10, right: 30, bottom: 30, left: 60},
        width = 1060 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;

    // append the svg object to the body of the page

    var svg = createSVG(width, height, margin)

    // Convert to integer
    formatDate(data);
    // Rank max by specialty, year and town
    rankMax = d3.rollup(data, v => d3.max(v, d => d.rang), d => d.specialite,
                        d => d.ville, d => d.annee);

    // Set the titles
    createChoice(svg, rankMax, width, height);
// Plot
    plotSpe(allSpe[0], rankMax, svg, width, height);
}

// Read data
// Change in firefox  privacy.file_unique_origin to false for local development
const all = d3.dsv(";", "2020-2016.csv");

all.then(function(data) {
    plot(data);
});
