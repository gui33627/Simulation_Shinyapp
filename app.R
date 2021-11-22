
library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(learnr)
# library(shinyLP)
theme_set(theme_bw())

ui <- fluidPage(
  
  titlePanel("Simulation"),
  
  navlistPanel(
    "Introduction",
    tabPanel('Get Started!',
             h3('Welcome to the simulation app!'),
             p("This app will walk you through simulating a hypothetical  intervention study and the goal is for you to understand how to use simulation to examine if different causal inference methods are unbiased and efficient in estimating treatment effect."),
             p("Throughout the app, we will use a hypothetical real-world example to build your intuition and knowledge about the joys of simulation."), 
             p("You will have access to two very important hats: the ", tags$em("researcher hat"), " and ", tags$em("omniscient hat"), ". 
               The",  tags$em("researcher hat"), " is one you wear daily - you are in the real world and have normal human limitations. However, every once in a while, you will get to wear the ", tags$em("omniscient hat"), "  where you will transcend your feeble human mind and become an all-knowing and powerful being. 
               These two hats are very important when we simulate, and will come into play quite often throughout our journey."),
             p("Ready? Okay, let's get started!"),
             br(),br(),br(),
             fluidRow(column(width = 6, align = 'center',
                             img(src='researcher_hat.png',width="100%", height="150%")),
                      column(width = 6, align = 'center',
                             img(src='omniscient_hat.png', width="100%", height="150%")))
             ),
             
    tabPanel("Hypothetical Example", 
      tags$div(
      p('The following hypothetical example will link concepts step by step throughout the app:'),
      h4('The Dreaded Global History Regents Exam'),
      p('In New York State, the Global History regents is considered to be one of 
      the most difficult Regents exams. Regents exams are a series of exams that high 
      school students must take and pass in order to graduate.
        Students must score a 65 or higher to pass but may re-take as many times as necessary.  
        ')),
      tags$div(
      p("The Global History regents is hard primarily due to the sheer amount of memorization (it is fairly traumatizing). 
      Hypothetically, the New York State Education Department is starting an afterschool program to assist students who failed at the first attempt to take the exam again.
      Before the second attempt of the exam, half of them are randomly assigned to attend an afterschool program and the other half does not receive any additional help. 
      The half of students that receive extra tutoring in the afterschool program is defined as the treated group. 
      The goal is to estimate the effect of the afterschool program on average test scores for the retake of the Global History regents.")),
      tags$div(
      p("Since this is a hypothetical example, you physically don't have any data. This means you're going to have to ", tags$strong("simulate your data"), " to answer your question. 
        The key here is that when you simulate fake data, you wear the omniscient hat. That is, you start from a position where you know and understand everything about how your data are generated and what's going on internally. ")),
      h4('The Whats and Whys of Simulation'),
      p("Simulation is a way to explore and understand things about your data, model(s), and underlying assumptions in a controlled environment. In this realm, you can switch freely between the researcher and omniscient hats, and see things about your data that you would normally never be able to see in the real world. This makes simulation a powerful learning tool when comparing different methods and examining efficiency and bias in causal inference."),
      p("Suppose the true causal effect (or", tags$em("treatment effect"), ") of the afterschool program is 
               an increase of 5 points on the exam score. Wearing your", tags$em("researcher hat"), " you would never know this 'true' treatment effect.
               You still can use causal inference methods to", tags$em("estimate"), " the effect of the afterschool program."),
      p("With simulation, you shall wear the", tags$em("omniscient hat"), " and create data with the true causal effect through data
             generation process. Knowing the true causal effect, you shall test whether different causal inference methods are valid or not.")
      ),
    tabPanel("What is simulation?",
             
             h4('Outcomes, Potential Outcomes, and Pre- and Post-Treatment Scores'),
             p("Let's decompose the description of the hypothetical example, and put the ", tags$em("omniscient hat"), " on, since it feels good to know everything. 
               With this powerful hat, you know that you have a sample of 100 fake students (that you will generate), 
               who must be randomly assigned (by some model you will specify) into treatment and control groups. 
               'Treatment' in this case is the afterschool program, and thus those in the treatment group will go through the afterschool program, 
               and those in the control group will receive normal tutoring. You will also be generating all of these students' 'pre-treatment' test scores (again, through models), as well as their 'post-treatment' test scores, otherwise known as the ", tags$em("outcome."),),
             p("These are fairly self-explanatory: ", tags$em("pre-treatment"), " test scores are the scores of the 100 sample students prior to any of them going through the afterschool program, and ", tags$em("post-treatment"), 
               " test scores are the scores of the 100 sample students after the treatment period. 
               The great thing about being omniscient is that you will also be able to see what the treatment group students' scores are if they don't receive the treatment - these are called", tags$em("potential outcomes.")),
             h4('Reality vs. Simulation'),
             p("For comparison, let's switch to the ", tags$em("researcher hat"), " for a moment to see the difference. As a mere researcher, you would still see the post-treatment scores for everyone, but you cannot know what the post-treatment test scores of the same treatment group students would be ", tags$em("if they hadn't received the treatment"), " (unless you can time travel, which you obviously can't do). 
            For instance, if the plots below show the post-treatment scores for each student if they participate in the program and if they do not. As a researcher, you can only observe one of those potential outcomes for each student."),
             # new mini-simulation
      sidebarLayout(
             sidebarPanel(
               radioButtons("potential_oc", "Potential Outcomes",
                            c("All potential outcomes" = "ally0y1",
                              "If everyone participates in the program" = "ally1",
                              "If everyone does not participate in the program" = "ally0",
                              "Researcher's point of view" = "obsy")
                            )
               ),
             mainPanel(plotOutput('researcher_hat_plot') 
                       )
      ),
      textOutput('researcher_hat_list'),
             #bookmark
          
              p("The beauty of simulation is that it allows you to overcome this meta-physical roadblock to create a sort of 'parallel universe' where, everything else being exactly the same, students in the treatment group never received the treatment. This is key to making causal inference."),
             p("For now, understand that simulation starts all the way at the beginning: who is in your sample, and what are their pre-treatment test scores?"),
             br(),
             h4('Example: Which students in your sample participate in the afterschool program?'),
             p("Click the button below to randomly select 50 students into the treatment group. 
                If you click again, you will select a different group of 50 students who participate in the program. 
                Each of your click generates a simulated sample of students. If you click many times, you can generate many simulated samples. 
                By combining all these samples or all possible senarios, we will be able to know the population trend.
                Try it for yourself!"),
             br(),
             p('All 100 students: Afiq, Leonor, Himari, Mary, Andrew, Dalisay, Michael, Sarah, Karen, John, Nancy, Lee, Mohammed, Ahmad, Aadya, Mark, Matthew, Daniel, Nur, Francisco, Analyn, Michelle, James, Emma, Camila, Lisa, Elizabeth, Hee-jung, Leon, Joshua, Nathan, Edward, Akari, Aarav, Joseph, Emily, William, Jacob, Ashley, Patricia, Ben, Salomé, Donna, Lucas, Juan, Anthony, Raphaël, Brian, Jennifer, Jessica, Sofía, Dorothy, Ren, Charles, Marco, Paul, George, Kevin, Kenneth, Megan, Andrea, Gabriel, Robert, Sandra, Mia, Yinuo, Steven, Maria, Hannah, Simon, Donald, Richard, Margaret, Chloé, Charlotte, Yichen, Kimberly, Melissa, Haruto, David, Olivia, Nathaniel, Barbara, Nurul, Oliver, Amanda, Giulia, Yuxuan, Carol, Yeri, Linda, Chiara, Saanvi, Thomas, Samantha, Alex, Xinyi, Jack, Christopher, Santiago.'),
             actionButton("draw_50_student", "Draw 50 students for treatment"),
             textOutput('student_list'),
             br(),
             p("Since the only limitation of simulation is computational, it can generate as many new results as you need, 
               and represents the characteristics of the model(s) you defined. 
               Now that you have a taste of what simulation looks like in its simplest form, 
               let's take a short step back to really understand the underlying models and their distributional properties, 
               since these really make up the core of your simulation study.")),
    
    
    "Probability Distribution",
    tabPanel("Sample and Population",
             img(src = "population-and-sample.png", height="40%", width="80%", align="center"),
             br(), br(), br(),
             p('In our Global History regents exam example, we have a sample of 100 students randomly drawn from the population of all high school students in New York State. 
               The100 students in the sample are the subjects you will collect data from/simulate data for.'),
             p('Typically, a population is too large to make a complete enumeration of all the individuals in the population. 
               Samples therefore are collected and statistics are calculated from the samples, 
               so that one can make inferences/generalizations from the sample to the population. 
               Specifically, using the treatment effect estimated from the 100 students sample,
               you will infer the treatment effect of the afterschool program on average for high school students in New York State.')),
    tabPanel("Discrete Random Variable and Distribution",
             h4('Probability Distribution'),
             p("With the sample of 100 students, you would like to first assign them to either the treatment group or the control group. 
               To do so, you need a data generator governed by a probability distribution. 
               Probability distributions are statistical functions that describe the likelihood of obtaining possible values that a random variable can take."),
             br(),
             h4('Bernoulli distribution'),
             p('When randomly assigning a student to a group, there are two possible values, treatment group or control group, 
               and each assignment of a student is independent of assignments of other students. 
               Such a probability distribution is called a Bernoulli distribution.'),
             p('There is only one parameter in Bernoulli distribution, probability of success p. In our example, we
               take the value 1 (treatment group) as success, with probability p, 
               the value 0 (control group) thus has a probability of (1-p).'),
             br(),
             fluidRow(column(width = 8,
                             h4('Illustration')),
                      column(width = 4, align = 'center',
                             img(src='omniscient_hat.png', width="30%", height="50%"))),
             
             p('Each click will show an assignment of a random student.
             Set the probability of the treatment group from 0 to 1 to see how often a student is in the treatment group.'),
             p('(Hint:if you set the p as 1, you assign all students to the treatment group.)'),
             sliderInput(inputId = "bernoulli_prob",
                         label = "Select the probability of assigning to the treatment group (p):",
                         min = 0, max = 1, value = 0.5, step = 0.1),
             actionButton("one_student_treatment", "Assign a student to a group"),
             br(),
             textOutput('one_student_treatment_plot'),
             br(), br(),
             p("Now let's run a Bernoulli trial for each of the 100 students. 
               Each of your clicks on the button 'Assign 100 students' will randomly re-assign each student to either treatment group (1) or control group (0)."),
             br(), 
             actionButton('reassign_100_treatment', "Assign 100 students"),
             br(), br(),
             plotlyOutput('animation_bernoulli'),
   
             
             tags$div(
               br(),
            h4('Binomial distribution'),
             p("Suppose you want to randomly assign treatments to 100 students, but do not need to know the exact roster for each group, 
               then you can use a binomial distribution to generate the data."),
             p("A Binomial distribution is a set of Bernoulli trials (when each trial is independent).
               There are two parameters in Binomial distribution, the number of Bernoulli trials, n, 
               and the probability of success in each trial, p. 
               In other words, a Binomial distribution is the number of successes in Bernoulli trials, and a Bernoulli distribution is when n=1 for a Binomial distribution. "),
            br(),
            fluidRow(column(width = 8,
                            h4('Illustration')),
                     column(width = 4, align = 'center',
                            img(src='omniscient_hat.png', width="30%", height="50%"))),
            p("Each click will simulate a result that assigns all the students in the sample to two groups. 
              The table summarizes the total counts of students in each group based on your selected number of students and probability of assigning to the treatment group."),
            sliderInput(inputId = "select_n_binomial",
                         label = "Select the number of treatment assignments (n):",
                         min = 1, max = 100, value = 100, step = 1),
             sliderInput(inputId = "select_p_binomial",
                         label = "Select the probability of in the treatment group in one assignmnet (p):",
                         min = 0, max = 1, value = 0.5, step = 0.1),
             actionButton("hundred_student_treatment", "Assign students"),
             textOutput('hundreds_student_treatment_result'),
             #plotOutput('hundred_students_treatment_plot'),

             tableOutput('hundreds_student_treatment_result_table'),
             br(),
             tags$div(
               useShinyjs(),
               actionButton("show_code_binomial", "Show me the R code of generating the distribution"),
               hidden(
                 div(id='code_div_binomial',
                     verbatimTextOutput("code_binomial")
                 )
               )
             ),
            br(),
            h4('In summary'),
             p('Both Bernoulli distribution and Binomial distribution are examples of probability distrubtion of discrete variables.', tags$strong('Discrete random variables'), ' can only take on a countable number of values (possibly infinite, but oftentimes finite), 
               such as treted to control group in an treatment assignment, or the number of students assigned to the treatment group.'),
             br(),  br(),  
             br())),
    tabPanel("Continuous Random Variable and Distribution",
             withMathJax(),
             h4('Normal Distribution'),
             p('For this hypothetical study, you next need to generate pre-treatment scores and post-treatment scores to estimate the effect of the afterschool program. 
             To simulate the two continuous variables, Normal distribution would be an appropriate probability distribution. 
               There are two parameters in normal distribution, a mean and a standard deviation of the variable.'),
             p('
              Normal distribution is a continuous probability distribution, 
              and it is often used in simulation and teaching because it approximates to many natural events. 
              There are two parameters in normal distribution, a mean and a standard deviation of the variable. '),
             fluidRow(column(width = 8,
                             h4('Illustration')),
                      column(width = 4, align = 'center',
                             img(src='omniscient_hat.png', width="30%", height="50%"))),
             #VZ-will use latex for the equations
             #do we need to explain var?
             p('You can pick the mean value, the expectation of the pre-treatment score (E(X)), and standard deviation of pre-treatment scores. '),
             withMathJax(paste0(' The mean value is the average score of all the high school students in New York. Since our sample is randomly collected, 
                we would also expect that our sample has the average score. 
                The mean is also called the expectation or expected value and is written as E(X) or \\(\\mu_X\\). The standard deviation of the distribution of X can be expressed as \\(\\sqrt(E((X − \\mu_X )^2))\\). ')),
             p('As you set different values for the mean and standard deviation of pre-treatment score, you may observe the center of your graph shifts and the spread of your graph changes.'),
             
             sliderInput(inputId = "select_mean_normal",
                         label = "Select the expectation of the pre-treatment score (E(X)):",
                         min = 20, max = 80, value = 60, step = 1),
             sliderInput(inputId = "select_sd_normal",
                         label = "Select the standard deviation of the pre-treatment score:",
                         min = 0, max = 10, value = 5, step = 1),
             
             actionButton("draw_hundred_student", "Simulate 100 students' scores"),
             br(),
             textOutput('hundred_student_score_print'),
             br(),
             plotOutput('hundred_students_scores'),
             br(),
             
             tags$div(
               useShinyjs(),
               actionButton("show_code_normal", "Show me the R code of generating the distribution"),
               hidden(
                 div(id='code_div_normal',
                     verbatimTextOutput("code_normal")
                 )
               )
             ),
             textOutput('normal_mean_var'),
             br(),
             h4('In summary'),
             p("We've learned that there are two types of random variables: ", tags$strong("discrete"), ' and ', tags$strong("continuous"), '. 
               Discrete random variables can only take on a countable number of values while continuous random variables can take on any real number, an uncountable amount of possibilities (i.e., to any amount of decimal places).')
             ),
    tabPanel("Exercise",
             htmlOutput("Exercise_1")),
  
    "Sampling Distribution",
    tabPanel("What is Sampling Distribution?",
             p('Suppose you simulated many samples consisting of 100 students randomly drawn from all the students from New York State, and with each sample you calculate a sample mean for 100 pre-treatment scores in order to estimate the population mean or expectation of pre-treatment score in New York State.
             A sample mean estimate from one sample is likely to be different from the sample mean estimate from another sample, and these sample means might be higher and lower than the true population mean. 
             The sampling distribution of sample mean is the set of possible sample means estimated from all samples of size 100 that could have been observed if the data simulation process had been re-done, along with the probabilities of these possible values.'),
             withMathJax(paste0("However, the combinations of 100 students from all students in New York State is an extraordinarily large number, and can even exceed the computation capacity of your computer. 
               For example, say there are 100,000 high school students in New York State and we randomly select 100 students. Here we have population size of 100,000 and sample size of 100. 
               How many samples of size 100 are possible out of a population of size 100,000? That's 100,000 choose 100, \\(100,000 \\choose100\\), and the number is so large that even R only returns Inf.")),
             br(),
             code("choose(100000,100)"),
             br(),
             verbatimTextOutput('sampling_distr'),
             p('Therefore, we usually use a large number of samples to get an approximate sampling distribution of statistics. 
               For example, below you can simulate a sampling distribution of sample mean by choosing the number of samples, and the population mean and standard deviation of the pre-treatement score.'),
             fluidRow(column(width = 8),
                      column(width = 4, align = 'center',
                             img(src='omniscient_hat.png', width="30%", height="50%"))),
             fluidRow(column(width = 6,
                             sliderInput(inputId = "select_n_sampling_distribution",
                                         label = "Select the number of samples to generate the sampling distribution:",
                                         min = 1000, max = 10000, value = 5000, step = 10)),
                      column(width = 6,
                             sliderInput(inputId = "select_mean_normal_sampling",
                                         label = "Select the expectation of the pre-treatment score (E(X)):",
                                         min = 20, max = 80, value = 60, step = 1),
                             sliderInput(inputId = "select_sd_normal_sampling",
                                         label = "Select the standard deviation of the pre-treatment score:",
                                         min = 0, max = 10, value = 5, step = 1))),
             
             actionButton("generate_sampling_distribution", "Generate the Sampling Distribution"),
             plotOutput('sampling_distribution_normal'),
             
             ## TODO: possibly talk about the mean and sd of sampling distribution
             
             p('If you are interested in the proportion of students who got into the treatment group, 
                you can also generate a sampling distribution for it by calculating the proportions for each of the many 100 students samples. 
                A proportion is a special case of an average in which the data are 1’s and 0’s (in the afterschool program/not in the afterschool program).'),
             fluidRow(column(width = 8),
                      column(width = 4, align = 'center',
                             img(src='omniscient_hat.png', width="30%", height="50%"))),
             tags$div(
               fluidRow(column(width = 6,
                               sliderInput('select_iter_bernoulli_sampling', label = "Select the number of samples to generate the sampling distribution:", min = 1000, max = 10000, value = 5000, step = 10)),
                        column(width = 6,
                               sliderInput('select_sample_size_bernoulli_sampling', label = "Select the number of Bernoulli trials in each sample:", min = 5, max = 1000, value = 100, step = 1),
                               sliderInput('select_prob_bernoulli_sampling', label = "Select the probability of success in each Bernoulli trial:", min = 0, max = 1, value = 0.5, step = 0.1) )),
               actionButton("generate_sampling_distribution_bernoulli", "Generate the Sampling Distribution"),
               plotOutput('sampling_distribution_bernoulli')),
             
             p('You may notice that the sampling distributions are all bell-curve no matter which distribution the statistics are originally calculated from. 
               This is actually summarized as a theorem called "Central Limit Theorem". Suppose that a sample is obtained containing many observations, 
               each observation being randomly generated in a way that does not depend on the values of the other observations, 
               and that the arithmetic mean of the observed values is computed. If this procedure is performed many times, 
               the central limit theorem says that the probability distribution of the average will closely approximate a normal distribution. ')),
            
      tabPanel("Exercise",
               htmlOutput("Exercise_2")),
  
    "Simulation",
    tabPanel("Data Generation Process (DGP)",
             fluidRow(column(width = 8,
                             h3('The Data Generating Process (DGP)')),
               column(width = 4, align = 'center',
                             img(src='omniscient_hat.png', width="40%", height="60%"))),
             p("In this final section, you will use what you've learned and simulated in the previous sections to answer the question you were initially tasked with at the beginning: 
               Is the afterschool program effective in improving high school students' scores on the Global History regents exam? Remember that omniscient hat? It's time to put it on."),
             p("As with any simulation study, you need to first establish the", tags$strong("Data Generating Process (DGP)."), "
               This means explicitly stating how you will be generating all of the data you need to estimate the treatment effect later on. 
               For the purposes of this study, we will use what we learned in previous sections to walk through our DGP."),
             h4('Treatment Assignment'),
             p("You already know how to simulate treatment assignments from Section 2 (Probability Distribution - Discrete Random Variables) using the Bernoulli distribution. The probability of assignment will be 0.5 for each of the 100 students."),
             verbatimTextOutput('simulation_treatment_code'),
             tags$div(
               useShinyjs(),
               actionButton("simulation_treatment", "Show every assignment"),
               hidden(
                 div(id='simulation_treatment_list',
                     textOutput("simulation_treatment_assign")
                 )
               )
             ),
             h4("Pre-treatment test scores"),
             p("You also know that you can use the Normal distribution from Section 2 (Probability Distribution - Continuous Random Variables) to simulate our pre-treatment test scores. 
               Remember: these are the original test scores of all the students prior to any of them attending the afterschool program."),
             verbatimTextOutput('simulation_prescore_code'),
             tags$div(
               useShinyjs(),
               actionButton("simulation_prescore", "Show every prescore"),
               hidden(
                 div(id='simulation_prescore_list',
                     textOutput("simulation_prescore_assign")
                 )
               )
             ),
             h4('Treatment Effect'),
             sliderInput(inputId = "tau", label = "Treatment effect:", min = -10, max = 10, value = 5, step = 1),
             
             h4('Outcome test scores based on treatment assignment'),
             textOutput('simulation_dgp_outcome'),
             
             fluidRow(column(width = 6,
                             sliderInput(inputId = "select_b0", label = "Intercept (b0):", min = 1, max = 20, value = 10, step = 1),
                             sliderInput(inputId = "select_b1", label = "Coefficient on X (b1):", min = 0.1, max = 1.2, value = 1, step = 0.1)
             ),
             column(width = 6,
                    sliderInput(inputId = "epsilon_error",
                                label = "Residual Std Dev (sigma)",
                                min = 0, max = 3, value = 1, step = 0.1),
                    checkboxInput(inputId = "include_y0", 
                                  label = "Show Y0", value = T),
                    checkboxInput(inputId = "include_y1",
                                  label = "Show Y1", value = T),
                    checkboxInput(inputId = "include_y0_mean", 
                                  label = "Show the mean function for Y0", value = T),
                    checkboxInput(inputId = "include_y1_mean",
                                  label = "Show the mean function for Y1", value = T))
             
             ),
             plotOutput(outputId = "result_plot", height = "500px"),
             verbatimTextOutput('simulation_postscore_code'),
             textOutput('simulation_postscore')
    ),
    tabPanel("Average Treatment Effect (ATE)",
            h3("Average Treatment Effect (ATE)"),
            p('Once you have simulated all the data necessary from our DGP, you can move on to estimating the', tags$strong("Average Treatment Effect (ATE)"),  
              'of the afterschool program using different causal inference methods. You would do this first by', tags$em("estimating"), 'the ATE while wearing the researcher hat, and then', 
              tags$em("calculating"), 'the true ATE while wearing the omniscient hat. Afterwards, you would compare the two to see how close our estimate is to the truth.'),
            textOutput('simulation_ate'),
            br(),
            fluidRow(column(width = 8,
                            h4('Calculate the true SATE')),
                     column(width = 4, align = 'center',
                            img(src='omniscient_hat.png', width="30%", height="50%"))),
            verbatimTextOutput('simulation_sate_code'),
            textOutput('simulation_sate'),
            fluidRow(column(width = 8,
                            h4('Use a difference in mean outcomes to estimate SATE')),
                     column(width = 4, align = 'center',
                            img(src='researcher_hat.png', width="30%", height="50%"))),
            verbatimTextOutput('simulation_mean_diff_code'),
            textOutput('simulation_mean_diff'),
            fluidRow(column(width = 8,
                            h4('Use Linear Regression to estimate SATE')),
                     column(width = 4, align = 'center',
                            img(src='researcher_hat.png', width="30%", height="50%"))),
            verbatimTextOutput('simulation_reg_code'),
            textOutput('simulation_reg')
),
    tabPanel("Estimator Comparisons",
             h3("Comparing Estimators"),
             p('Now you will further explore the properties of these two different approaches to estimating our ATEs by simulation. 
               For now we will only consider the variability in estimates that would manifest as a result of the randomness in who is assigned to receive the treatment (this is sometimes referred to as “randomization based inference”). 
               Since you are wearing an omniscient hat you can see how the observed outcomes and estimates would change across a distribution of possible treatment assignments. 
               You simulate this by repeatedly drawing a new vector of treatment assignments and then for each new dataset calculating estimates using our two estimators above.'),
             verbatimTextOutput('mean_diff_reg_compare'),
             plotOutput('mean_diff_compare'),
             plotOutput('reg_compare'),
             h5('Standardized bias using mean difference method:'),
             verbatimTextOutput('mean_diff_biasedness_code'),
             textOutput('mean_diff_biasedness'),
             h5('Standardized bias using regression method:'),
             verbatimTextOutput('reg_biasedness_code'),
             textOutput('reg_biasedness'),
             h5('Efficiency using mean difference method:'),
             verbatimTextOutput('mean_diff_efficiency_code'),
             textOutput('mean_diff_efficiency'),
             h5('Efficiency using regression method:'),
             verbatimTextOutput('reg_efficiency_code'),
             textOutput('reg_efficiency')
          
             
             ), # use sampling distribution to compare unbiasedness and efficiency
    tabPanel("Exercise",
         htmlOutput("Exercise_3"))
))



server <- function(input, output, session) {
  students <- sample(c('James','Robert', 'John',
                'Michael', 'William', 'David', 'Richard', 'Joseph', 'Thomas', 'Charles', 'Christopher', 'Daniel', 
                'Matthew', 'Anthony', 'Mark', 'Donald', 'Steven', 'Paul', 'Andrew', 'Joshua', 'Kenneth', 'Kevin', 
                'Brian', 'George', 'Edward', 'Mary', 'Patricia', 'Jennifer', 'Linda', 'Elizabeth', 'Barbara',
                'Yeri', 'Jessica', 'Sarah', 'Karen', 'Nancy', 'Lisa', 'Lee', 'Margaret', 'Sandra', 'Ashley',
                'Kimberly', 'Emily', 'Donna', 'Michelle', 'Dorothy', 'Carol', 'Amanda', 'Melissa', 'Hee-jung',
                'Yichen','Aarav', 'Mohammed',
                'Sofía', 'Olivia', 'Lucas', 'Ben', 'Emma', 'Mia', 'Chloé', 'Gabriel', 'Raphaël', 
                'Santiago', 'Francisco', 'Leonor', 'Leon', 'Maria', 'Himari', 'Nathaniel', 'Jacob', 'Dalisay', 'Analyn', 
                'Nur', 'Yuxuan', 'Ahmad', 'Megan', 'Charlotte', 'Xinyi', 'Jack', 'Alex', 'Giulia',
                'Andrea', 'Chiara', 'Marco', 'Hannah', 'Samantha', 'Nathan', 'Simon', 'Camila', 'Juan', 'Afiq',
                'Nurul', 'Haruto', 'Ren', 'Akari', 'Salomé', 'Oliver', 'Aadya', 'Saanvi', 'Yinuo'))
  #bookmark

  #mini-set dataset
  pre=rnorm(n = 10, mean = 50, sd = 5)
  y0=10 + pre + 0 + rnorm(10, mean = 0, sd = 1)
  y1=10 + pre + 5 + rnorm(10, mean = 0, sd = 1)
  treat=rbinom(n = 10, size = 1, prob = 0.5)
  y=ifelse(treat == 1,  y1, y0)
  rhdf<-data.frame(
    students=sample(students,10),
    pre=pre,
    y0=y0,
    y1=y1,
    treat=treat,
    y=y)
  pbase<-ggplot(rhdf) +theme_bw()+theme(legend.position = "none")+ylim(40,80)
  
  p01<-pbase+geom_point(aes(students, y0)) +
    geom_point(aes(students, y1),color = "red")
  p1<-pbase+geom_point(data =rhdf, aes(students, y1),color = "red")
  p0<-pbase+geom_point(aes(students, y0)) 
  py<-pbase+geom_point(aes(students, y,colour= factor(treat)))+
    scale_color_manual(values=c("black","red"))
  
  output$researcher_hat_plot<- renderPlot({
    potential_oc <- switch(input$potential_oc,
                           ally0y1 = p01,
                           ally1 = p1,
                           ally0 = p0,
                           obsy = py,
                           p01)
    potential_oc
  })

  output$researcher_hat_list<-renderText(paste0(rhdf$students, ': ', rhdf$treat))
  
  
  
   observeEvent(input$draw_50_student, {
    studentlist <- sample(students, size = 50)
    output$student_list <- renderText(toString(studentlist))
  })
  
  observeEvent(input$one_student_treatment, {
    treatment <- rbinom(1,size = 1, prob = input$bernoulli_prob)
    student <- sample(students, size = 1)
    output$one_student_treatment_plot <- renderText(paste0(student, ': ', treatment))
  })
  
  df <- reactiveValues(treatment = c(), score = c())
  
  observeEvent(input$hundred_student_treatment, {
   
    df$treatment <- rbinom(1, size = input$select_n_binomial, prob = input$select_p_binomial)
    # df$treatment <- rbinom(sample_size, size = 1, prob = input$select_p_binomial)
    # text <- c()
    # for (i in 1:sample_size) {
    #   text <- c(text, paste0(students[i], ': ', df$treatment[i]))
    # }
    # print(text)
    # output$hundreds_student_treatment_result <- renderText(toString(text))
    
    # output$hundred_students_treatment_plot <- renderPlot({
    #   tmp <- data.frame(treatment = df$treatment)
    #   ggplot() + geom_histogram(data = tmp, aes(x = treatment, y = ..density..), bins = 30, alpha = 0.5) 
    # })
    table <- data.frame(Group = c('treatment (1)','control (0)'), Frequence = c(as.integer(df$treatment), as.integer(input$select_n_binomial - df$treatment)))
    output$hundreds_student_treatment_result_table <- renderTable(table)
  })
  
  
  observeEvent(input$draw_hundred_student, {
    df$score <- rnorm(100,input$select_mean_normal,input$select_sd_normal)
    text <- c()
    for (i in 1:100) {
      text <- c(text, paste0(students[i], ': ', round(df$score[i])))
    }
    output$hundred_student_score_print <- renderText(toString(text))
    
    output$hundred_students_scores <- renderPlot({
      tmp <- data.frame(score = df$score)
      mean <- paste0('Sample Mean: ', round(mean(as.numeric(tmp$score)),1))
      sd <- paste0('Sample Standard Deviation: ', round(sd(as.numeric(tmp$score)),1))
      ggplot() + geom_histogram(data = tmp, aes(x = score, y = ..density..), bins = 30, alpha = 0.5) + 
        geom_vline(xintercept = mean(as.numeric(tmp$score)), color = 'blue') +
        annotate("text",x=input$select_mean_normal + 10,y=0.095,label= mean, fontface = "italic", size = 5) +
        annotate("text",x=input$select_mean_normal + 10,y=0.085,label= sd, fontface = "italic", size = 5) 
    })
  })
  
  output$normal_mean_var <- renderText({
    tmp <- data.frame(score = df$score)
    mean <- round(mean(as.numeric(tmp$score)),1)
    sd <- round(sd(as.numeric(tmp$score)),1)
    
    text <- paste0("The plot above shows the distribution of the pre-treatment test scores of students in the 100 students sample. 
    When you wear omnicient hat, you specify and hence know the true expectation and standard deviation of the pre-treatment score of all students in New York State. 
    However, in reality we only have a sample of 100 students and can only estimate the expectation by the sample mean of the 100 pre-treatment scores and estimate the standard deviation by the sample standard deviation of the 100 pre-treatment scores.
    The sample mean score of the 100 students is ", mean, ", and the sample standard deviation of students' scores is ", sd, ".")
    
  })

  observeEvent(input$show_code_normal, {
    toggle('code_div_normal')
    output$code_normal <- renderText({
      paste0('rnorm(n = 100, mean = ', input$select_mean_normal, ', sd = ', input$select_sd_normal, ')')
    })
    
  })
  
  observeEvent(input$show_code_binomial, {
    toggle('code_div_binomial')
    output$code_binomial <- renderText({
      paste0('# n specifies the number of Binomial distribution \n# size defines how many Bernoulli trials in a Binomial distribution \n# prob prescribes the probability of success in one Bernoulli trial \nrbinom(n = 1, size = ', input$select_n_binomial, ', porb = ', input$select_p_binomial, ')')
    })
    
  })
  

  
  
  Z_100 <- reactiveValues(data = rbinom(100, size = 1, prob = 0.5))
  
  observeEvent(input$reassign_100_treatment, {
    Z_100$data <- rbinom(100, size = 1, prob = 0.5)
  })
  
  output$animation_bernoulli <- renderPlotly({
    
    Z <- Z_100$data
    stage1 <- data.frame(id = 1:100, name = students, frame = rep(1, 100), x = rep(0.5, 100), y = 1:100, z = rep('before', 100))
    stage2 <- data.frame(id = 1:100, name = students, frame = rep(2, 100), x = ifelse(Z == 1, 1, 0), y = 1:100, z = ifelse(Z == 1, 'treat', 'control'))
    
    df <- rbind(stage1, stage2)
    t <- list(
      family = "sans serif",
      size = 7,
      color = toRGB("grey50"))
    
    plot_ly() %>% 
      add_markers(data=df, x = ~x, y = ~y, frame = ~frame, ids = ~id, text = ~name, hoverinfo = "text") %>%
      animation_opts(frame = 2000, transition = 1999, redraw = FALSE) %>% 
      add_text(data=df, x = ~x, y = ~y, frame = ~frame, ids = ~id, text = ~name, textfont = t, textposition = "middle right") %>% 
      animation_opts(frame = 2000, transition = 1999, redraw = FALSE) %>% 
      layout(
        title = 'Treatment assignment for each of the 100 students',
        font=list(
          family='Times New Roman',
          size=12),
        xaxis = list(
          title = "Group",
          ticktext = list("0", "1"), 
          tickvals = list(0, 1),
          tickmode = "array"),
        showlegend = FALSE)
  })
  
  
  output$sampling_distr <- renderText(choose(100000,100))
  
  
 
    output$sampling_distribution_normal <- renderPlot({
      input$generate_sampling_distribution
      select_n <- isolate(input$select_n_sampling_distribution)
      select_mean <- isolate(input$select_mean_normal_sampling)
      select_sd <- isolate(input$select_sd_normal_sampling)
      all_means <- data.frame(data = rep(NA, select_n))
      for (i in 1:select_n) {
        sample <- rnorm(n = 100, mean = select_mean, sd = select_sd)
        tmp <- mean(sample)
        all_means$data[i] <- tmp
      }
      ggplot() + geom_histogram(data = all_means, aes(x = data, y = ..density..), bins = 30, alpha = 0.5) +
        geom_vline(xintercept = mean(all_means$data), color = 'blue') 
      
    })

  
  
    output$sampling_distribution_bernoulli <- renderPlot({
      input$generate_sampling_distribution_bernoulli
      select_iter <- isolate(input$select_iter_bernoulli_sampling)
      select_p <- isolate(input$select_prob_bernoulli_sampling)
      select_n <- isolate(input$select_sample_size_bernoulli_sampling)
      proportions <- data.frame(data = rep(NA, select_iter))
      for (i in 1:select_iter) {
        tmp <- rbinom(n = select_n, size = 1, prob = select_p)
        proportions$data[i] <- mean(tmp)
      }
      ggplot() + geom_histogram(data = proportions, aes(x = data, y = ..density..), bins = 30, alpha = 0.5) +
        geom_vline(xintercept = mean(proportions$data), color = 'blue') 
    })
    
  
  
    Z <- rbinom(n = 100, size = 1, prob = 0.5)
    X <- rnorm(n = 100, mean = 50, sd = 5)
    
    Y0 <- reactive({
      input$select_b0 + input$select_b1*X + rnorm(100, mean = 0, sd = input$epsilon_error)
    }) 
    Y1 <- reactive({input$select_b0 + input$select_b1*X + input$tau + rnorm(100, mean = 0, sd = input$epsilon_error) }) 
    Y <- reactive({
      ifelse(Z == 1,  Y1(), Y0())
    })
    
    output$simulation_treatment_code <- renderText({
      "rbinom(n = 100, size = 1, prob = 0.5)"
    })
    

    output$simulation_prescore_code <- renderText({
      "rnorm(n = 100, mean = 50, sd = 5)"
    })

    
    observeEvent(input$simulation_treatment, {
      toggle('simulation_treatment_list')
      text <- c()
      for (i in 1:100) {
        text <- c(text, paste0(students[i], ': ', round(Z[i])))
      }
      output$simulation_treatment_assign <- renderText(toString(text))
    })
    observeEvent(input$simulation_prescore, {
      toggle('simulation_prescore_list')
      text <- c()
      for (i in 1:100) {
        text <- c(text, paste0(students[i], ': ', round(X[i])))
      }
      output$simulation_prescore_assign <- renderText(toString(text))
    })
    
    output$simulation_dgp_outcome <- renderText({
      paste0("As omniscient beings, you know that the treatment effect (or tau is ", input$tau, 
        ". That is, you know that the post-treatment test scores of students who went through the afterschool program is on average ", input$tau, " points higher than the students who did not. 
               To generate these outcome scores, you would simulate a dependency based on the treatment assignment variable from above. 
               In the interactive graph below, you can specify the true relationship between the pre-treatment test scores (X) and the outcome test scores by selecting the coefficients in the regression model.")
    })
    
    # generate real relationship 
    real_functional_relationship <- reactive({
      data <- data.frame(x = X, z=Z)
      data$y0_actual <-  with(data, x*input$select_b1 + input$select_b0)
      data$y1_actual <-  with(data, x*input$select_b1 + input$tau + input$select_b0)
      data
    })
    
    sample_tibble <- reactive({
      data <- data.frame(x = X, z = Z)
      data$y0 <-  Y0()
      data$y1 <-  Y1()
      data
    })
    
    output$result_plot <- renderPlot({
      req(input$select_b0)
      req(input$select_b1)
      colors <- c("Y0 (control)" = "blue", "Y1(treated)" = "red")
      data <- paste0(" Y0 = ", input$select_b0, " + ", input$select_b1, "X + e, ", "e~N(0, ",input$epsilon_error,"^2)\n", "Y1 = ", input$select_b0, " + ", input$select_b1, "X + ", input$tau," + e, ", "e~N(0, ",input$epsilon_error,"^2)")
      final_plot <- ggplot() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(data), fontface = "italic", size = 6) + scale_color_manual('Potential Outcomes', values = colors) 
      if(input$include_y0){
        final_plot <- final_plot + geom_point(data = sample_tibble(), aes(x = x, y = y0, color = 'Y0 (control)')) 
      }
      if(input$include_y1){
        final_plot <- final_plot + geom_point(data = sample_tibble(), aes(x = x, y = y1, color = 'Y1(treated)')) 
      }
      if(input$include_y0_mean){
        final_plot <- final_plot + geom_line(data = real_functional_relationship(), aes(x = x, y = y0_actual, color = 'Y0 (control)'))
      }
      if(input$include_y1_mean){
        final_plot <- final_plot + geom_line(data = real_functional_relationship(), aes(x = x, y = y1_actual, color = 'Y1(treated)'))
      }
      
      final_plot
    })
    
    output$simulation_postscore_code <- renderText({
      paste0("tau <- ", input$tau," \nY0 <- ",input$select_b0," + ",input$select_b1,"X + 0 + rnorm(100, mean = 0, sd = ", input$epsilon_error, ") \nY1 <- ",input$select_b0, " + ", input$select_b1, "X + tau + rnorm(100, mean = 0, sd = ", input$epsilon_error,") \nY <- ifelse(Z == 1,  Y1, Y0)")
    })
    
    output$simulation_postscore <- renderText({
      text <- c()
      for (i in 1:100) {
        text <- c(text, paste0(students[i], ': ', round(Y()[i])))
      }
      toString(text)
    })
    
    output$simulation_ate <- renderText({
      paste0('Note that we use "estimate" when you wear the researcher hat and use "calculate" when you wear the omniscient hat. 
               This is intentional because as a researcher, you will never know the truth (in this case, that the treatment effect is ', input$tau,') and thus you are always estimating the ATE (or any other estimand). 
               But when you are simulating and omniscient, you will always be calculating, since you know the true treatment effect.')
    })
    
    output$simulation_sate_code <- renderText({
      "mean(Y1 - Y0)"
    })
    
    output$simulation_sate <- renderText({
      round(mean(Y1() - Y0()), 2)
    })
    
    
    output$simulation_mean_diff_code <- renderText({
      "mean(Y[Z == 1]) - mean(Y[Z == 0])"
    })
    
    output$simulation_mean_diff <- renderText({
      round(mean(Y()[Z == 1]) - mean(Y()[Z == 0]), 2)
    })
    
    output$simulation_reg_code <- renderText({
      "fit <- lm(Y ~ X + Z) \nsummary(fit)$coefficients['Z', 1]"
    })
    
    output$simulation_reg <- renderText({
      fit <- lm(Y() ~ X + Z)
      round(summary(fit)$coefficients['Z', 1], 2)
    })
    
    output$mean_diff_reg_compare <- renderText({
      "mean_diff <- c() \nlm_estimate <- c() \nN <- 100 \nfor (i in 1:5000) {\n    Z <- rbinom(N, 1, prob = 0.5) \n    Y <- ifelse(Z == 1, Y_1, Y_0)
    \n    mean_diff_tmp <- mean(Y[which(Z == 1)]) - mean(Y[which(Z == 0)]) \n    fit_tmp <- lm(Y ~ X + Z) \n
    lm_estimate_tmp <- coef(fit_tmp)['Z'] \n    mean_diff <- c(mean_diff, mean_diff_tmp) \n    lm_estimate <- c(lm_estimate, lm_estimate_tmp)
}"
    })
    
    
    
    
    comparison <- reactive({
      mean_diff <- c()
      lm_estimate <- c()
      N <- 100
      for (i in 1:5000) {
        Z <- rbinom(N, 1, prob = 0.5)
        Y <- ifelse(Z == 1, Y1(), Y0())
        mean_diff_tmp <- mean(Y[which(Z == 1)]) - mean(Y[which(Z == 0)])
        fit_tmp <- lm(Y ~ X + Z)
        lm_estimate_tmp <- coef(fit_tmp)['Z']
        mean_diff <- c(mean_diff, mean_diff_tmp)
        lm_estimate <- c(lm_estimate, lm_estimate_tmp)
      }
      result <- list(mean_diff, lm_estimate)
    })
    
    SATE <- reactive({mean(Y1() - Y0())})
    
    output$mean_diff_compare <- renderPlot({
      cols <- c("True SATE" = "red", "Mean" = "blue")
      mean_diff_df <- data.frame(data = comparison()[[1]])
      ggplot() + geom_histogram(data = mean_diff_df, aes(x = data, y = ..density..), bins = 30, alpha = 0.5, col = 'black') +
        geom_vline(aes(xintercept = mean(as.numeric(mean_diff_df$data)), color = 'Mean')) +
        geom_vline(aes(xintercept = SATE(), color = 'True SATE')) + xlim(min(mean_diff_df$data) - 0.1, max(mean_diff_df$data) + 0.1) +
        labs(title = 'Distribution of Mean Difference', x = 'Mean Difference', y = 'Frequency') +
        scale_color_manual(values = cols) +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))
    })
    
    output$reg_compare <- renderPlot({
      cols <- c("True SATE" = "red", "Mean" = "blue")
      lm_estimate_df <- data.frame(data = comparison()[[2]])
      ggplot() + geom_histogram(data = lm_estimate_df, aes(x = data, y = ..density..), bins = 30, alpha = 0.5, col = 'black') +
        geom_vline(aes(xintercept = mean(as.numeric(lm_estimate_df$data)), color = 'Mean')) +
        geom_vline(aes(xintercept = SATE(), color = 'True SATE')) + xlim(min(comparison()[[1]]) - 0.1, max(comparison()[[1]]) + 0.1) +
        labs(title = 'Distribution of Regression Estimate', x = 'Regression Estimate', y = 'Frequency') +
        scale_color_manual(values = cols) +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))
    })
    
    output$mean_diff_biasedness_code <- renderText({
      '(mean(mean_diff)-SATE)/sd(Y)'
    })
    output$mean_diff_biasedness <- renderText({
      (mean(comparison()[[1]])-SATE())/sd(Y())
    })
    
    output$reg_biasedness_code <- renderText({
      '(mean(lm_estimate)-SATE)/sd(Y)'
    })
    output$reg_biasedness <- renderText({
      (mean(comparison()[[2]])-SATE())/sd(Y())
    })
    
    output$mean_diff_efficiency_code <- renderText({
      'sd(mean_diff)'
    })
    output$mean_diff_efficiency <- renderText({
      sd(comparison()[[1]])
    })
    
    output$reg_efficiency_code <- renderText({
      'sd(lm_estimate)'
    })
    output$reg_efficiency <- renderText({
      sd(comparison()[[2]])
    })
    
    output$Exercise_1<- renderUI({
      tags$iframe(width = "1000", height = "900",
             src = "https://verazhouty.shinyapps.io/Exercise-ProbDist/")
    })
  
    output$Exercise_2<- renderUI({
      tags$iframe(width = "1000", height = "900",
             src = " https://verazhouty.shinyapps.io/Exercise-SamplingDist/")
    })
    
    output$Exercise_3<- renderUI({
      tags$iframe(width = "1000", height = "900",
             src = "https://verazhouty.shinyapps.io/Exercise-Simulation/")
    })
}

shinyApp(ui, server)


