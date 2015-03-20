#SHINY UI SCRIPT
#Where the layout of the shiny app is decided

shinyUI(
  
  navbarPage("R SHINY",
             inverse= TRUE,
             
             #Home tab panel
             tabPanel("Home",
                      img(src="trinity.jpg"),
                      h3("Frequently used statistical 
                               analysis methods using R-Shiny"),
                      h5("A Fourth year Project for the Department Of Statistics")
                      
             ),
             
             #T-test tab panel
             tabPanel("T-Test",
                      
                      tabsetPanel(
                        tabPanel("Introduction",
                                 h4("Introduction"),
                                 hr(),
                                 p("The t-test looks at the t-statistic, t-distribution and degrees of 
                                   freedom to determine a p value (probability) that can be used to determine 
                                   whether the population means differ. The t-test is one of a number of hypothesis tests."),
                                 hr(),
                                 #text about hypothesis tests and the t distribution
                                 p("For any Hypothesis test we follow these steps:"),
                                 p("Step 1: State the null and alternative hypothesis."),
                                 p("Step 2: Determine the level of significance."),
                                 p("Step 3: Compute the test statistic."),
                                 p("Step 4: Make a decision and draw conclusions"),
                                 hr(),
                                 p("In the case of t-tests we are interested in 
                                    the behaviour of the sample mean. While theory tells us that this is normally
                                    distributed. We also factor in uncertainty about the
                                    sample deviation. For this reason we use a t-distribution"),
                                 hr(),
                                 
                                 sidebarLayout(
                                   sidebarPanel(sliderInput("dfslide", "Degrees of Freedom", 
                                                            min=0, max=30, value=3),
                                                p("You can see the T-distribution here in red and the normal distribution
                                                    in black. See what happens to the T-distribution as you 
                                                  raise its degrees of freedom by moving the slider.")),
                                   mainPanel(plotOutput("df"))
                                 )
                                 
                                 
                                 
                        ),
                        
                        tabPanel("Paired T-Test", 
                                 h4("Paired T-Test"),
                                 p("The Paired T-Test is used when you have two groups which 
                                             are correlated in some way in this example the dataset shows the effect
                                              of two soporofic drugs (increase in hours of sleep compared to control) on 10 patients"),
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     "The Sleep dataset shows the effect 
                                                 of two soporific drugs (increase in
                                                 hours of sleep compared to control) 
                                                 on 10 patients. ",
                                     tableOutput("sleep")
                                     
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("The Dataset",
                                                wellPanel(
                                                  p("The Dataset we will use in this example is the sleep dataset
                                                            which comes built in to R and is visible on the left."),
                                                  p("The Sleep dataset shows the effect 
                                                             of two soporific drugs (increase in
                                                             hours of sleep compared to control) 
                                                             on 10 patients."),
                                                  p("If you would like more information on the sleep dataset
                                                            enter ",
                                                    code("?sleep"),
                                                    "into your R console or click ",
                                                    a("here", 
                                                      href = "https://stat.ethz.ch/R-manual/R-patched/library/datasets/html/sleep.html"))
                                                ),
                                                selectInput("tbox", 
                                                            label = "Plots of the data",
                                                            choices = c( "Boxplots of both groups","Boxplot of difference of two groups"),
                                                            selected = "Boxplots of both groups"),
                                                
                                                plotOutput("Tboxplot")
                                                
                                                
                                       ),
                                       tabPanel("The T statistic",
                                                p("The T statistic is calculated using the following formula"),
                                                withMathJax(),
                                                h2(' $$ t =\\frac{\\bar{d}}{\\frac{s}{\\sqrt{n}}}$$'),
                                                wellPanel(
                                                  p("Where"),
                                                  
                                                  h5("$$\\begin{align}
                                                               t &=  \\text{the t-statistic} \\\\
                                                               \\bar{d} &= \\text{The mean difference of the 2 groups} \\\\
                                                               s &=  \\text{The Standard deviation of the difference of the 2 groups} \\\\
                                                               n &=  \\text{The population size}
                                                               \\end{align}$$")
                                                  
                                                ),
                                                p("so we get"),
                                                h3(' $$ t =\\frac{-1.58}{\\frac{1.2299}{\\sqrt{10}}}$$'),
                                                h3('$$ t = -4.06$$')
                                                
                                       ),
                                       
                                       tabPanel("The P value and Critical value",
                                                h3("P Value"),
                                                p("Formally the P value is the probability of observing an event
                                                            more extreme than the data. We can calculate this using the
                                                            T-statistic and the degrees of freedom and the pt() function in R"),
                                                p("The T-statistic we calculated was -4.06 and there were 9 degrees of freedom"),
                                                p("so we can calculate the P value by entering the following code in to R"),
                                                code("pt(4.06, 9, lower.tail= FALSE)"),
                                                p("but our test is two tailed so we should multiply it by 2 so we enter"),
                                                code("2*pt(4.06, 9, lower.tail= FALSE)"),
                                                p("This gives us a p value of "),
                                                verbatimTextOutput("pvalue"),
                                                hr(),
                                                h3("Critical value"),
                                                p("We can find the critical value by using our tables books or
                                                            using the qt() function in R"),
                                                p("We calculate using the tables book by going to the table seen below and
                                                            by finding where the degrees of freedom and our alpha value intersect "),
                                                p("In this case we have degrees of freedom = 9 and an alpha value of .05 for a 95% confidence 
                                                            interval"),
                                                img(src="critical.jpg"),
                                                p("We can also calculate this using R by entering the following line of code "),
                                                code("qt(0.025, 9, lower.tail= FALSE)"),
                                                br(),
                                                br(),
                                                verbatimTextOutput("crit"),
                                                h3("Conclusions"),
                                                p("Our T-statistic is 4.06 which is well outside the critical value of 2.26 so we can reject the
                                                            null hypothesis."),
                                                p("This is represented graphically below as you can see the critical value represented in red and 
                                                            the t statistic in green."),
                                                plotOutput("tstat")
                                                
                                                
                                                
                                       ),
                                       tabPanel("Calculating Using R",
                                                
                                                p("R has a handy T-test function which we can use to get the same results"),
                                                p("The function in R is called t.test()"),
                                                p("to get more information on t.test() enter ?t.test() in to your R console or click",
                                                  a("here", 
                                                    href = "https://stat.ethz.ch/R-manual/R-patched/library/stats/html/t.test.html")),
                                                p("Here is a copy of the R code used to obtain the output below"),
                                                code("t.test(extra ~ group, data = sleep, paired=TRUE)"),
                                                hr(),
                                                verbatimTextOutput("paired")
                                                
                                                
                                       )
                                     )
                                     
                                   )
                                   
                                 )
                        )
                        
                        
                      )
             ),
             
             #Chi Square tab panel
             tabPanel("Chi-Square",
                      tabsetPanel(
                        #Chi Square Introduction
                        tabPanel("Introduction",
                                 wellPanel(
                                   p("A Chi square test is a statistical test 
                                         commonly used to compare observed data with
                                         data we would expect to obtain according to
                                         a specific hypothesis"),
                                   p("represented by the below formula"),
                                   
                                   #use math jax package to display the formulas
                                   withMathJax(),
                                   
                                   
                                   h3("$$\\chi^2=\\sum_{i=1}^I \\sum_{j=1}^J \\frac{(O_{ij}-E_{ij})^2}{E_{ij}} $$"),
                                   
                                   
                                   p("Where"),
                                   
                                   p("$$\\begin{align*}
                                        \\chi^2 &=  \\text{Test Statistic}\\\\
                                        I &= \\text{# number of rows in contingency table}\\\\
                                        J &=  \\text{# number of columns}
                                       \\end{align*}$$"),
                                   
                                   p("Chi square tests are commonly used to test
                                         if deviations in expected values are due to 
                                         chance or by other contributing factors")
                                   
                                   
                                   
                                 ) ),
                        
                        #Chi Square data
                        tabPanel("Students Admissions at UC Berkeley",
                                 
                                 h4("Student Admissions at UC Berkeley"),
                                 
                                 p("This dataset of student admissions at Berkeley University in 1973 is
                                         a famous example of simpsons paradox and can also be used to demonstrate 
                                         a Chi Square test"),
                                 
                                 
                                 hr(),
                                 #sidebar panel showing gender and admissions
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("At first glance it appears that the university 
                                             has a clear sex bias in favour of males as there seems
                                             to be a much higher acceptance rate of males compared to females.
                                              "),
                                     p("Males have an acceptance rate of 45% compared to females who have
                                             an acceptance rate of 30%")
                                   ),
                                   mainPanel(
                                     tableOutput("data"),
                                     
                                     tableOutput("proptable")
                                     
                                   )
                                 ),
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("The Chi square test for Gender vs Admission is also highly significant suggesting
                                             a bias based on Gender.")
                                   ),
                                   mainPanel(
                                     verbatimTextOutput("chi_sa")
                                   )
                                 ),
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("However if we look at Gender vs Department for each individual department you
                                             can see that there is only significence for department a"),
                                     selectInput("pick", 
                                                 label = "Department",
                                                 choices = c( "A","B","C",
                                                              "D","E","F"),
                                                 selected = "A")),
                                   mainPanel(
                                     verbatimTextOutput("chi_dept")
                                     
                                   )
                                   
                                 ),
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("However, if we test Admissions against Department, results are again
                                             highly significant.")
                                   ),
                                   mainPanel(
                                     verbatimTextOutput("chi_ad")
                                     
                                   )
                                   
                                 ),
                                 
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("Similarly for Department against Gender")
                                   ),
                                   mainPanel(
                                     verbatimTextOutput("chi_sd")
                                     
                                   )
                                   
                                 ),
                                 
                                 hr()
                                 
                                 
                        ),
                        
                        #Chi Square explanation and output
                        tabPanel("A Closer Look",
                                 h4("A Closer Look:"),
                                 p("A number of Chi Square Tests were conducted using the chisq.test() function"),
                                 hr(),
                                 
                                 
                                 #sidebar panel showing gender and admissions for departments
                                 sidebarLayout(
                                   sidebarPanel("But when you take a closer look at the data you can see that
                                                      actually more females apply for the departments with higher 
                                                      rejection rates and in general the acceptance rates for males
                                                      and females are very similar and sometimes even favours females."),
                                   mainPanel(
                                     tableOutput("data1"),
                                     tableOutput("data2")
                                   )),
                                 
                                 hr(),
                                 
                                 #well panel of mosaic plots for each department
                                 wellPanel(
                                   sidebarLayout(
                                     
                                     sidebarPanel(selectInput("mo", 
                                                              label = "Admissions for Department",
                                                              choices = c( "A","B","C",
                                                                           "D","E","F"),
                                                              selected = "A"),
                                                  p("A nice way to graphically display the data is by using mosaic
                                                        plots. Mosaic plots are suitable in this instance as they clearly
                                                        display the proportions of admissions so are easily comparable."),
                                                  p("You can see male students in blue and female in red")
                                                  
                                     ),
                                     mainPanel(plotOutput("mosaicA"))
                                     
                                   )),
                                 
                                 hr()
                                 
                                 
                                 
                        )
                        
                      )
                      
             ),
             
             #Anova tab panel
             tabPanel("ANOVA",
                      tabsetPanel(
                        tabPanel("Introduction",
                                 h4("Intoduction:"),
                                 p("ANOVA or Analysis of Variance allows for comparison of multiple groups at once"),
                                 hr(),
                                 p("The basic idea underlying the simplest form of ANOVA is that the variation between
                                    individual observations can be viewed as having two components: within-group and
                                    between-group."),
                                 p("The within-group component is seen as pure chance variation - withingroup
                                    conditions are held constant and there are no reasons why any two observations
                                    should be different, apart from the influence of the chance variation that affects the
                                    system being studied."),
                                 p("Between-group variation, on the other hand, is seen as (at least potentially) systematic:
                                    different groups are different, or are treated differently, in ways that may lead to higher
                                    or lower average responses."),
                                 p("The statistical test for between-group differences is based on the
                                    ratio of the two mean squares: MS(Between-group)/MS(Within-group) - if this is large, it
                                    suggests systematic between-group differences")
                               
                                 
                        ),
                        tabPanel("Data",
                                 h4("Data:"),
                                 p("For this example we will look at a dataset of Boron measurements for the same adhesive taken
                                   from four different labs. The aim of the study was to test if there was any systematic variance 
                                   between the Boron measurement techniques in each lab"),
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("the six replicates were measured on
                                        different days, so the variation between them reflects medium term chance
                                        measurement error within the laboratories. The Boron measurements are take at the parts per million(ppm) level")
                                     
                                     ),
                                   mainPanel(
                                     tableOutput("boron")
                                     
                                     )
                                   ),
                                 hr(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("without any formal statistical analysis it is obvious from looking at the data graphically that the average
                                        results in laboratories 3 and 4 are different; it is not clear, however, whether laboratories
                                        1 and 2 differ by more than the chance analytical day-to-day variation which is clearly
                                        present in all laboratories.")
                                   ),
                                   mainPanel(
                                     plotOutput("Anova_dot")
                                   )
                                 ),
                                 hr(),
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("The data displayed graphically with a boxplot")
                                   ),
                                   mainPanel(
                                     plotOutput("Anova_box")
                                   )
                                 ),
                                 hr()
                                 
                                 
                                 ),
                        tabPanel("Decomposing the Variance",
                                 h4("Decomposing the Variance:"),
                                 p("We will now run through the technical side of decomposing the variance "),
                                 hr(),
                                 withMathJax(),
                                 
                                 h3("$$\\text{SS Total} = \\text{SS Within} + \\text{SS Between} $$"),
                                 
                                 h3("$$\\sum_{i=1}^I \\sum_{j=1}^J (y_{ij}-\\bar y)^2 =
                                    \\sum_{i=1}^I \\sum_{j=1}^J (y_{ij}-\\bar y i.)^2 +
                                    \\sum_{i=1}^I \\sum_{j=1}^J (\\bar y_{i.}-\\bar y)^2
                                    $$"),
                                 hr(),
                                 h3("$$\\text{DF Total} = \\text{DF Within} + \\text{DF Between} $$"),
                                 h3("$$IJ-1 = I(J-1)+I-1$$"),
                                 hr()
                                
                                 
                                 ),
                        tabPanel("ANOVA Table",
                                 h4("The ANOVA TABLE:"),
                                 p("A Look at the ANOVA table function in R and its output"),
                                 hr(),
                                 verbatimTextOutput("Anova_table")
                                 
                                 )
                      )
                      
                      
             ),
             
             #Regression tab panel
             tabPanel("Regression",
                      tabsetPanel(
                        tabPanel("Introduction",
                                 h4("Intoduction:"),
                                 p("Regression analysis is, perhaps, the most important data modelling technique in statistics."),
                                 hr(),
                                 p("Regression is concerned with fitting equations to data. In the simplest case this means that we have a variable X which we believe is related to a second variable Y and which we wish to use to predict Y;"),
                                 p("the relationship between the two variables is believed to be linear. We do not assume that X is a perfect predictor of Y: in the systems we study there is always chance variation, which prevents us from making perfect predictions."),
                                 wellPanel(
                                   h4("The Basic Regression Formula"),
                                   h3("$$Y_i = \\beta_0 + \\beta_1 X_i + \\varepsilon_i       $$")
                                 
                                 )
                                 
                                 ),
                        tabPanel("Data",
                                 h4("Data"),
                                 hr(),
                                 p("The dataset used in this section is a dataset of spot welds with their Strength and Diameter"),
                                 hr(),
                                 tableOutput("welds")
                                 
                                 
                                 
                                 ),
                        tabPanel("Regression",
                                 plotOutput("Regression"),
                                 hr(),
                                 checkboxInput("reg", label = "Regression Line", value = FALSE),
                                 p("The Regression line can be thought of as an ‘average line’ through the data – at any given
                                    diameter, the strength of replicate welds will vary about this line; this is the line
                                    that (approximately) joins up the means of the strength values for different weld
                                    diameters."),
                                 hr(),
                                 checkboxInput("pre", label = "Prediction Interval", value = FALSE),
                                 p("The prediction interval represents where we predict with 95% certainty the strength for a single
                                   spot weld diameter would fall"),
                                 hr(),
                                 checkboxInput("con", label = "Confidence Interval", value = FALSE),
                                 p("The confidence interval represents where we are we are 95% confident that the interval covers the true long-run mean
                                    at this strength")
                                 
                                 
                                 
                                 
                                 
                                 )
                      )
             ),
             
             #File Upload tab panel
             tabPanel("File Upload",
                      h4("File Upload"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Choose CSV file to upload',
                                    accept = c(
                                      'text/csv',
                                      'text/comma-separated-values',
                                      'text/tab-separated-values',
                                      'text/plain',
                                      '.csv',
                                      '.tsv'
                                    )
                          )
                        ),
                        mainPanel(
                          tableOutput('upload'),
                          h1(textOutput("warning1"))
                          )
                        
                      )
                      
               ),
             
             #load css theme 
             includeCSS("background.css")             
                
  ))
