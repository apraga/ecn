
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
        .domain([new Date(2015, 0), new Date(2020, 0)])
        .range([ 0, width ]);
}

function updateLinesPoints(svg, name, active) {
    var opacity = active ? 1 : 0.1;
    svg.select("path[class=\"" + name+ "\"]")
        .transition().style("opacity", opacity);
    svg.select("g[class=\"" + name + "\"]").selectAll("circle")
        .transition().style("opacity", opacity);
    var t = svg.select("text[id=\"" + name + "\"]").transition();
    t.style("font-weight", active ? 900 : 0) // Bold or normal
    t.style("text-decoration", active ? "underline" : "") // Underlined or normal
}

function legendNb() { return 7; }

// X-Position of a given label (numbered i) in the legend
function legendX(d, i) {
    return (i % legendNb()) *150;
}

// Y-Position of a given label (numbered i) in the legend
function legendY(d, i) {
    return plotHeight() +myMargin().bottom + Math.floor(i/legendNb())*(legendHeight()/4);
}

function createCheckboxes(svg, towns, myColor) {
    //    For redraw
    d3.selectAll("label").remove()
    d3.selectAll("input").remove()

    // Add a "check all" option
    d3.select("body")
        .append('label').text("Tous")
        .append('input')
        .attr("name", "fullSelect")
        .attr("type", "checkbox")
        .on("click", function(d){
            var check = this.checked
            towns.forEach(function (d) {
                updateLinesPoints(svg, d, check);
            })
        })

    // Labels must be positionned inside the SVG to control their position (x and y)
    svg.selectAll("mylabels") // we have to select all 
        .data(towns)
        .enter()    
        .append('text')
        .attr('x', legendX)
        .attr('y', legendY)
        .text(function(d) { return d; })
        .attr('id',function(d,i){return d})
	.style("fill", function(d){return myColor(d) })
	.style("font-weight", 0)
        // On click, show the current line and change text to bold
        .on("click", function(d){
            // This.__data__ is a bit ugly to get the name but it works
            updateLinesPoints(svg, this.__data__, ! isMarked(this.style))
        })


    // Only show first value: set the
    svg.selectAll("path[class="+towns[0]+"]").style("opacity", 1); // Set path
    svg.selectAll("g[class="+towns[0]+"]").selectAll("circle").style("opacity", 1); // Set circles
    d3.select("input[id=legend0]").property("checked", true) // Checkbox
}

// True if the legend has been clicked on (so in bold and underlined)
function isMarked(style) {
    return style["font-weight"] == 900;
}


function plotSpe(speTitle, rankmax, svg) {
    // Speciality is no longer a variable
    spe = rankMax.get(speTitle);

    // // Get the list of town for categories
    towns = Array.from(spe.keys()).sort();

    // Date  = X axis
    var x = setXAxis(plotWidth());
    svg.append("g")
        .attr("transform", "translate(0," + plotHeight() + ")")
        .call(d3.axisBottom(x).ticks(d3.timeYear.every(1)));

    // Rank = Y axis
    var y = setYAxis(plotHeight());
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

    // create a tooltip
    var Tooltip = svg.append("g")
        .append("text")
        .attr("font-size", 15)

    // Move text according to the mouse
    // https://observablehq.com/@d3/multi-li
    var moveTooltip = function(d) {
        // From the parameter, we get the circle position
        // It's a bit hacky but the other choice is to use bisect (should be more expensive)
        // We don't have to mess with mouse position !

        const date = d.target.__data__[0];

        // Also, if we have only one specialty checked we should display results for that !
        // It avoids wrong results where plots are too closo te each other
        // Could not managed the filter without an array (.nodes)...
        // Also, we cannot used the checked property directly...
        var match = d3.selectAll("input[type=checkbox]").nodes();
        var matchF = match.filter(function(v) {return v.checked;})
        if (matchF.length == 1) {
            rank = spe.get(matchF[0].__data__).get(date);
        }
        else
            rank = d.target.__data__[1];

        Tooltip.attr("transform", `translate(${x(date)}, ${y(rank)})`)
        Tooltip.text(rank)
    }

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
        .on("mousemove", moveTooltip)

        createCheckboxes(svg, towns, myColor);
}

function createChoice(svg, rankMax){
    d3.select("body").selectAll("h2").html("Rang maximal pour <select id=\"selectSpe\"></select>");
    allSpe = Array.from(rankMax.keys()).sort();


    var defaultChoice = "Pédiatrie";
    // add the options to the drow-down list
    d3.select("#selectSpe")
        .selectAll('myOptions')
        .data(allSpe)
        .enter()
        .append('option')
        .text(function (d) { return d; }) // text shown in the menu
        .attr("value", function (d) { return d; }) // corresponding value return
        .property("selected", function(d) { return d === defaultChoice}) // change default value

    // When the button is changed, run the updateChart function
    d3.select("#selectSpe").on("change", function(d) {
        // Don't forget to clean svg !!
        svg.selectAll('*').remove();
        var speTitle = d3.select(this).property("value")
        plotSpe(speTitle, rankMax, svg);
    })

// Plot
    plotSpe(defaultChoice, rankMax, svg);
}

function createSVG() {
    var margin = myMargin();
    return d3.select("#myplot")
        .append("svg")
        .attr("width", plotWidth() + margin.left + margin.right)
        .attr("height", plotHeight() + legendHeight() + margin.top + margin.bottom)
        .append("g")
        .attr("transform",
              "translate(" + margin.left + "," + margin.top + ")");
}

// set the dimensions and margins of the graph
function myMargin() {
    return {top: 10, right: 100, bottom: 30, left: 60};
}
function plotWidth() {
    return 1060 - myMargin().left - myMargin().right;
}
function plotHeight() {
    return 700 - myMargin().top - myMargin().bottom;
}
function legendHeight() {
    return 100;
}

function plot(data){
    // append the svg object to the body of the page
    var svg = createSVG();

    // Convert to integer
    formatDate(data);
    // Rank max by specialty, year and town
    rankMax = d3.rollup(data, v => d3.max(v, d => d.rang), d => d.specialite,
                        d => d.ville, d => d.annee);

    // for (let t of rankMax.values()) {
    // 	for (let d of t.values()) {
    // 	    for (let r of d.values()) {
    // 		if (r == 0) {
    // 		    console.log(d);
    // 		    // const i = d.indexOf(r);
    // 		    // d.splice(i,1);
    // 		}
    // 	    }
    // 	}
    // }

    // Set the titles
    createChoice(svg, rankMax);

}

// Read data
// Change in firefox  privacy.file_unique_origin to false for local development
const all = d3.dsv(";", "ranks.csv");

all.then(function(data) {
    plot(data);
});
