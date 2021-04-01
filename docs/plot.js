//Many thanks to https://www.d3-graph-gallery.com/graph/connectedscatter_multi.html
//
function formatDate(data) {
    data.forEach(function(x) {
        x.annee = new Date(+x.annee, 0);
        x.rang = +x.rang;
    })
}

function plot(data){
    // set the dimensions and margins of the graph
    var margin = {top: 10, right: 30, bottom: 30, left: 60},
        width = 1060 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;

    // append the svg object to the body of the page
    var svg = d3.select("#myplot")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");

    // Convert to integer
    formatDate(data);
    // Rank max by specialty, year and town
    rankMax = d3.rollup(data, v => d3.max(v, d => d.rang), d => d.specialite,
                        d => d.ville, d => d.annee);
    // Speciality is no longer a variable
    spe = rankMax.get("génétique médicale");

    // // Get the list of town for y-axis
    towns = Array.from(spe.keys());

    // Date  = X axis
    var x = d3.scaleTime()
    /* .domain(d3.extent(spe, function(d) { return d[1]; })) */
        .domain([new Date(2016, 0), new Date(2020, 0)])
        .range([ 0, width ]);

    svg.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x).ticks(d3.timeYear.every(1)));

    // Rank = Y axis
    var y = d3.scaleLinear()
        .domain([0, 8800]) // TODO
        .range([0, height]);

    svg.append("g")
    // .attr("transform", "translate(0," + height + ")")
        .call(d3.axisLeft(y));

    var myColor = d3.scaleOrdinal().domain(towns)
        .range(d3.schemeSet3);
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
        .attr("stroke", function(d){ return myColor(d[0]) })
        .style("stroke-width", 4)
        .style("fill", "none")

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

    // Interactive for new version ofD3
    // const tooltip = new Tooltip();

    // // Add a legend (interactive)
    // svg.selectAll("myLegend")
    //     .data(spe)
    //     .enter()
    //     .append('g')
    //     .append("text")
    //     .attr('x', function(d,i){ return 30 + i*60})
    //     .attr('y', 10)
    //     .text(function(d) { return d[0]; })
    //     .style("fill", function(d){ return myColor(d[0]) })
    //     .style("font-size", 15)

    // Generate checkbox
    d3.select("body").selectAll("input")
        .data(towns)
        .enter()
        .append('label')
        .attr('for',function(d,i){ return 'a'+i; })
        .text(function(d) { return d; })
        .style("fill", function(d){ return myColor(d[0]) }) // does not work :(
        .append("input")
        .attr("checked", true)
    // .attr("name", function(d) { return "name_" + d; }) // Store the name here for easy retrieval on clock
        .attr("type", "checkbox")
        .attr("id", function(d,i) { return 'a'+i; })
        .on("click", function(d){
            // This.__data__ is a bit ugly to get the name but it works
            svg.selectAll("." + this.__data__).transition().style("opacity", this.checked == 1 ? 1 : 0)
        })
}

// Read data
// Change in firefox  privacy.file_unique_origin to false for local development
const all = d3.dsv(";", "2020-2016.csv");

all.then(function(data) {
    plot(data);
});
