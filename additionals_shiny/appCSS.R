# CSS modifications
# old colors: body & th background color #003E6E
# well colors: #07ACE7
# layout
# .irs ==> for slider
appCSS <- "
th {
padding-top: 12px;
padding-bottom: 12px;
text-align: left;
background-color: #59d1e8;
color: black;
}
body {
background-color: #59d1e8;
}
.well {
background-color:	#06afce;
border-color: #06afce;
border-radius: 1px;
}
.shinybusy-modal {
position:fixed;
top: 0;
left: 0;
background-color: #59d1e8;
color: black;
  margin: -15% auto;
  padding: 20px;
  border: 1px solid #888;
  width: 80%; 

}
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(20%);
             }
.btn-err {
margin-top: 10px;
color: red;
}
.col-sm-4 {width: 250px;}
.irs-from, .irs-to, .irs-single, .irs-grid-text { color: #000000 }
"
