
library(shiny)
library(ggplot2)
library(shinyjs)
theme_set(theme_bw())

ui <- fluidPage(
  
  titlePanel("Simulation"),
  
  navlistPanel(
    "Introduction",
    tabPanel('Get Started!',
             h3('Welcome to the simulation app!'),
             p("This app will walk you through simulating hypothetical samples to test if different causal inference methods are unbiased and efficient in estimating treatment effect."),
             p("Throughout the app, we will use a hypothetical real-world example to build your intuition and knowledge about the joys of simulation."), 
             p("You will have access to two very important hats: the ", tags$em("researcher hat"), " and ", tags$em("omniscient hat"), ". 
               The",  tags$em("researcher hat"), " is one you wear daily - you are in the real world and have normal human limitations. However, every once in a while, you will get to wear the ", tags$em("omniscient hat"), "  where you will transcend your feeble human mind and become an all-knowing and powerful being. 
               These two hats are very important when we simulate, and will come into play quite often throughout our journey."),
             p("Ready? Okay, let's get started!")),
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
      Hypothetically, the school is starting an afterschool program to assist students who failed at the first attempt to take the exam again.
      Before the second attempt of the exam, half of them is randomly assigned to attend an afterschool program and the other half does not receive any additional help. 
      The half of students that receives extra tutoring in the afterschool program is defined as the treated group. 
      The goal is to estimate the effect of the afterschool program on average test scores for the retake of the Global History regents.")),
      tags$div(
      p("Since this is a hypothetical example, you physically don't have any data. This means you're going to have to ", tags$strong("simulate your data"), " to answer your question. 
        The key here is that when you simulate fake data, you wear the omniscient hat. That is, you know and understand everything about how your data are generated and what's going on internally. "))),
    tabPanel("What is simulation?",
             h4('The Whats and Whys of Simulation'),
             p("Simulation is a way to explore and understand things about your data, model(s), and underlying assumptions in a controlled environment. In this realm, you can switch freely between the researcher and omniscient hats, and see things about your data that you would normally never be able to see in the real world. This makes simulation a powerful learning tool when comparing different methods and examining efficiency and bias in causal inference."),
             p("Suppose the true causal effect (or", tags$em("treatment effect"), ") of the afterschool program is 
               an increase of 5 points on the exam score. Wearing your", tags$em("researcher hat"), " you would never know this 'true' treatment effect.
               You still can use causal inference methods to", tags$em("estimate"), " the effect of the afterschool program."),
             p("With simulation, you shall wear the", tags$em("omniscient hat"), " and create data with the true causal effect through data
             generate process. Knowing the true causal effect, you shall test whether different causal inference methods are valid or not."),
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
             p("For comparison, let's switch to the ", tags$em("researcher hat"), " for a moment to see the difference. As a mere researcher, you would still see the post-treatment scores for everyone, but you cannot know what the post-treatment test scores of the same treatment group students would be ", tags$em("if they hadn't received the treatment"), " (unless you can time travel, which you obviously can't do). The beauty of simulation is that it allows you to overcome this meta-physical roadblock to create a sort of 'parallel universe' where, everything else being exactly the same, students in the treatment group never received the treatment. This is key to making causal inference."),
             p("For now, understand that simulation starts all the way at the beginning: who is in your sample, and what are their pre-treatment test scores?"),
             
             br(),
             h4('Example: Which students in your sample participate in the afterschool program?'),
             p("Click the button below to randomly select 50 students into the treatment group. 
                If you click again, you will select a different group of 50 students who participate in the program. 
                Each of your click generates a simulated sample of students. If you click many times, you can generate many simulated samples. 
                By combining all these samples or all possible senarios, we will be able to know the population trend.
                Try it for yourself!"),
             br(),
             p('All 100 students: James, Robert, John, Michael, William, David, Richard, Joseph, Thomas, Charles, Christopher, Daniel, Matthew, Anthony, Mark, Donald, 
               Steven, Paul, Andrew, Joshua, Kenneth, Kevin, Brian, George, Edward, Mary, Patricia, Jennifer, Linda, Elizabeth, Barbara, Yeri, Jessica, Sarah, Karen, 
               Nancy, Lisa, Lee, Margaret, Sandra, Ashley, Kimberly, Emily, Donna, Michelle, Dorothy, Carol, Amanda, Melissa, Hee-jung,Yichen,Aarav, Mohammed, Sofía, 
               Olivia, Lucas, Ben, Emma, Mia, Chloé, Gabriel, Raphaël, Santiago, Francisco, Leonor, Leon, Maria, Himari, Nathaniel, Jacob, Dalisay, Analyn,Nur, Yuxuan, 
               Ahmad, Megan, Charlotte, Xinyi, Jack, Alex, Giulia,Andrea, Chiara, Marco, Hannah, Samantha, Nathan, Simon, Camila, Juan, Afiq,Nurul, Haruto, Ren, Akari, 
               Salomé, Oliver, Aadya, Saanvi, Yinuo.'),
             
             actionButton("draw_50_student", "Draw 50 students for treatment"),
             textOutput('student_list'),
             br(),
             p("Since the only limitation of simulation is computational, it can generate as many new results as you need, 
               and represents the characterstics of the model(s) you defined. 
               Now that you have a taste of what simulation looks like in its simplest form, 
               let's take a short step back to really understand the underlying models and their distributional properties, 
               since these really make up the core of your simulation study.")),
    
    
    "Probability Distribution",
    tabPanel("Sample and Population",
             img(src = "population-and-sample.png", height = 400, width = 700),
             br(), br(), br(),
             p('As you have seen in the introduction, we sampled 100 students from all high school students in New York State. 
               In the example, the population is all high school students in New York State, 
               and the sample is the 100 students that you will collect data from.'),
             p('Typically, population is very large and making a complete enumeration of all the individuals in the population either impractical or impossible. 
               A sample usually represents a subset of manageable size. Samples are collected and statistics are calculated from the samples, 
               so that one can make inferences/generalizations from the sample to the population. 
               Specifically, we want to infer the treatment effect of the afterschool program on average for high school students in New York State 
               from the treatment effect estimated from the 100 students sample.')),
    tabPanel("Discrete Random Variable and Distribution",
             #VZ:do we want to put a demo graph for sampling? 
             #the one Jennifer suggested population to sample and how sample represents population
             #Up till this session, we yet to explain what is a random variable- do we need to or are we assuming our users would have known?
             h4('Probability Distribution'),
             p("With our sample of 100 students, we would like to assign them to the treatment group or the control group.
             To do so, we need a data generator governed by a probability distribution.
               Probability distributions are statistical functions that describe the likelihood of obtaining possible values that a random variable can take."),
             br(),
             h4('Bernoulli distribution'),
             p('
               When we randomly assign a student to a group, there are two possible values, treatment group or control group,
               and each assignment is independent of each other. Such probability distribution is called a Bernoulli distribution. '),
             p('There is only one parameter in Bernoulli distribution, probability of success p. In our example, we
               take the value 1 (treatment group) as success, with probability p, 
               the value 0 (control group) thus has a probability of (1-p).'),
             
             h4('Try for yourself!'),
             
             p('Each click will show an assignment of a random student.
             Set the probability of the treatment group from 0 to 1 and see how often a student is in the treatment group.'),
             p('(Hint:if you set the p as 1, every click will show a student being in the treatment group.)'),
             sliderInput(inputId = "bernoulli_prob",
                         label = "Select the probability of assigning to the treatment group (p):",
                         min = 0, max = 1, value = 0.5, step = 0.1),
             actionButton("one_student_treatment", "Assign a student to a group"),
             br(),
             textOutput('one_student_treatment_plot'),
   
             
             tags$div(
               br(),
            h4('Binomial distribution'),
             p("If we randomly assign treatments to 100 students, and suppose we do not need to know exactly roster for each group, we can use a binomial distribution to generate the data."),
             p("A Binomial distribution is a set of Bernoulli trials (when each trial is independent).
               There are two parameters in Binomial distribution, the number of Bernoulli trials, n, 
               and the probability of success for each event, p. 
               In other words, a Binomial distribution is the number of successes in Bernoulli trials, and a Bernoulli distribution is when n=1 for a Binomial distribution. "),
            h4('Try for yourself!'),
            p("Each click will simulate a result that assigned every student to a group. The table summarizes the total counts
            of students in each group based on your selected number of students and probability of assigning to the treatment group.
            "),
            sliderInput(inputId = "select_n_binomial",
                         label = "Select the number of treatment assignments (n):",
                         min = 1, max = 100, value = 100, step = 1),
             sliderInput(inputId = "select_p_binomial",
                         label = "Select the probability of in the treatment group in one assignmnet (p):",
                         min = 0, max = 1, value = 0.5, step = 0.1),
             actionButton("hundred_student_treatment", "Assign students"),
             textOutput('hundreds_student_treatment_result'),
             # plotOutput('hundred_students_treatment_plot'),
            #VZ-change table title
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
            h4('In summary'),
             p('Both Bernoulli distribution and Binomial distribution are examples of probability distrubtion of discrete variables.', tags$strong('Discrete random variables'), ' can only take on a countable number of values (possibly infinite, but oftentimes finite), 
               such as treted to control group in an treatment assignment, or the number of students assigned to the treatment group.'),
             br(),  br(),  
             br())),
    tabPanel("Continuous Random Variable and Distribution",
             h4('Normal Distribution'),
             p('For this hypothetical study, we also want to generate some covariates and outcomes to estimate the effects of the afterschool program.
              We will use the pre-treatment score as the example here.'),
             p('
              Normal distribution is a continuous probability distribution, 
              and it is often used in simulation and teaching because it approximates to many natural events. 
              There are two parameters in normal distribution, a mean and a standard deviation of the variable. '),
             h4('Try for yourself!'),
             #VZ-will use latex for the equations
             #do we need to explain var?
             p('You can pick the mean value, the expectation of the pre-treatment score (E(X)), and standard deviation of pre-treatment scores. '),
             p(' The mean value is the average score of all the high school students in New York. Since our sample is randomly collected, 
                we would also expect to our sample has the average score. 
                The mean is also called the expectation or expected value and is written as E(X) or mu_X. The standard deviation of the distribution of X can be expressed as sqrt(E((X − mu_X )^2)). '),
             p('As you set different values for the mean and standard deviation of pre-treatment score, you may observe the center of your graph shifts and the spread of your graph changes.'),
             
             sliderInput(inputId = "select_mean_normal",
                         label = "Select the expectation of the pre-treatment score (E(X)):",
                         min = 20, max = 80, value = 60, step = 1),
             sliderInput(inputId = "select_sd_normal",
                         label = "Select the standard deviation of the pre-treatment score:",
                         min = 0, max = 10, value = 5, step = 1),
             
             actionButton("draw_hundred_student", "Simulate 100 students' scores"),
             br(),
             #VZ-may change it to a table or a better format later
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
             #VZ-if we really need to explain var, maybe we better combine it with explaining how rnorm is used in epsilon?
             h4('Normal distribution assumption in regression assumptions'),
             p('In addition to the distribution of data, normal distributions is also commonly used in regression modeling to help us characterize the variation that remains after predicting the average ---
               the error term epsilon in the expression y = a + bx + epsilon. The distributions allows us to get a handle on how uncertain our predictions are and, additionally, our uncertainty in the estimated parameters of the model.'),
             br(),
             h4('In summary'),
             p("We've learned that there are two types of random variables: ", tags$strong("discrete"), ' and ', tags$strong("continuous"), '. 
               Discrete random variables can only take on a countable number of values while continuous random variables can take on any real number, an uncountable amount of possibilities (i.e., to any amount of decimal places).')),
    
             
    "Sampling Distribution",
    tabPanel("What is Sampling Distribution?",
             p('Suppose you simulated many samples consisting of 100 students drawn from all the students from New York State, and with each sample you calculate a sample mean for 100 pre-treatment scores in order to estimate the population mean or expectation of pre-treatment score in New York State.
             A sample mean estimate from one sample is likely to be different from the sample mean estimate from another sample, and these sample means might be higher and lower than the true population mean. 
             The sampling distribution of sample mean is the set of possible sample means estimated from all samples of size 100 that could have been observed if the data simulation process had been re-done, along with the probabilities of these possible values.'),
             p("However, the combinations of 100 students from all students in New York State is an exaodinary large number, and can even exceed the computation capacity of your computer. 
               For example, say there are 100,000 high school students in New York State and we randomly select 100 students. Here we have population size of 100,000 and sample size of 100. 
               How many samples of size 100 are possible out of a population of size 100,000? That's 100,000 choose 100, ${100,000 choose100}$, and the number is so large that even R only returns Inf."),
             br(),
             code("choose(100000,100)"),
             br(),
             verbatimTextOutput('sampling_distr'),
             p('Therefore, we usually use a large number of samples to get an approximate sampling distribution of statistics. 
               For example, below you can simulate a sampling distribution of sample mean by choosing the number of samples, and the population mean and standard deviation of the pre-treatement score.'),
             sliderInput(inputId = "select_n_sampling_distribution",
                         label = "Select the number of samples to generate the sampling distribution:",
                         min = 1000, max = 10000, value = 5000, step = 10),
             
             sliderInput(inputId = "select_mean_normal_sampling",
                         label = "Select the expectation of the pre-treatment score (E(X)):",
                         min = 20, max = 80, value = 60, step = 1),
             sliderInput(inputId = "select_sd_normal_sampling",
                         label = "Select the standard deviation of the pre-treatment score:",
                         min = 0, max = 10, value = 5, step = 1),
             actionButton("generate_sampling_distribution", "Generate the Sampling Distribution"),
             plotOutput('sampling_distribution_normal'),
             
             ## TODO: possibly talk about the mean and sd of sampling distribution
             
             p('If we are interested in the proportion of students who got into the treatment group, 
                we can also generate a sampling distribution for it by calculating the proportions for each of the many 100 students samples. 
                A proportion is a special case of an average in which the data are 1’s and 0’s (in the afterschool program/not in the afterschool program).'),
             tags$div(
               sliderInput('select_n_bernoulli_sampling', label = "Select the number of Bernoulli trials", min = 1000, max = 10000, value = 5000, step = 10),
               sliderInput('select_prob_bernoulli_sampling', label = "Select the probability of success in each Bernoulli trial", min = 0, max = 1, value = 0.5, step = 0.1),
               plotOutput('sampling_distribution_bernoulli')),
             
             p('You may notice that the sampling distributions are all bell-curve no matter which distribution the statistics are originally calculated from. 
               This is actually summarized as a theorem called "Central Limit Theorem". Suppose that a sample is obtained containing many observations, 
               each observation being randomly generated in a way that does not depend on the values of the other observations, 
               and that the arithmetic mean of the observed values is computed. If this procedure is performed many times, 
               the central limit theorem says that the probability distribution of the average will closely approximate a normal distribution. ')
             
             # p("One continuous predictor: y = b0 + b1x + eps"),
             # numericInput(inputId = "select_b0", "Intercept (b0):", 1),
             # numericInput(inputId = "select_b1", "Coefficient on X (b1):", 0.5),
             # numericInput(inputId = "select_sigma", "Residual Std Dev (sigma):", 1),
             # sliderInput(inputId = "sample_size",label = "Select Sample Size",
             #             min = 10, max = 1000, value = 250, step = 10),
             # plotOutput('regression'),
             # p("In practice, we will not know the sampling distribution; we can only estimate it, as it depends on aspects of the population, not merely on the observed data. In the pure random sampling model, the sampling distribution depends on all N datapoints. For the measurement-error model, 
             #   the sampling distribution depends on the parameters a, b, and $sigma$, which in general are not known, and will be estimated from the data."),
             # 
             # p("The simplest example of a sampling distribution is the pure random sampling model: if the data are a simple random sample of size n from a population of size N, then the sampling distribution is the set of all samples of size n, all with equal probabilities."),
             # p("The normal distribution, binomial distribution, and poisson distribution with specified parameters on the previous page are all sampling distributions for the samples of sizes of your choice."),
             # p("The next simplest example is pure measurement error: if observations $y_i$, i = 1,. . . , n, are generated from the model $y_i = a + bx_i + epsilon_i$ , with fixed coefficients a and b, pre-specified values of the predictor $x_i$ , and a specified distribution for the errors $epsilon_i$ 
             #   (for example, normal with mean 0 and standard deviation $sigma$), then the sampling distribution is the set of possible datasets obtained from these values of $x_i$ , 
             #   drawing new errors $epsilon_i$ from their assigned distribution.")
            ),
    "Simulation",
    tabPanel("Data Generation Process (DGP)",
             p("In this final section, we will use what we learned and simulated in the previous sections to answer the question you were initially tasked with at the beginning: 
               Is the afterschool program effective in improving high school students' scores on the Global History regents exam? Remember that omniscient hat? It's time to put it on."),
             p("As with any simulation study, we need to first establish our **Data Generating Process (DGP)**. 
               This means explicitly stating how you will be generating all of the data you need to estimate the treatment effect later on. 
               For the purposes of this study, we will use what we learned in previous sections to walk through our DGP."),
             h3('Treatment Assignment'),
             p("We already know how to simulate treatment assignments from [section 2] using the Bernoulli distribution. The probability of assignment will be 0.5 for each of the 100 students."),
             verbatimTextOutput('simulation_treatment_code'),
             textOutput('simulation_treatment'),
             h3("Pre-treatment test scores"),
             p("We also know that we can use the Normal distribution from [section 2] to simulate our pre-treatment test scores. 
               Remember: these are the original test scores of all the students prior to any of them attending the afterschool program."),
             verbatimTextOutput('simulation_prescore_code'),
             textOutput('simulation_prescore'),
             
             h3('Outcome test scores based on treatment assignment'),
             p("As omniscient beings, we know that the treatment effect (or \tau) is **5**. 
               That is, we know that the post-treatment test scores of students who went through the afterschool program is on average **5** points higher than the students who did not. 
               To generate these outcome scores, we would simulate a _dependency_ based on the treatment assignment variable from above:"),
             verbatimTextOutput('simulation_postscore_code'),
             textOutput('simulation_postscore')
          
             ),
    tabPanel("Average Treatment Effect (ATE)",
            p('Once we have simulated all the data necessary from our DGP, we can move on to estimate the aveargae treatment effect of the afterschool program using different causal inference methods 
               when we wear researcher hat in reality and compare them to the true average treatment effect which we know can calculate only if we wear a omnicient hat. 
               Note that we use "estimate" when you wear the researcher hat and use "calculate" when you wear the omniscient hat. 
               This is intentional because as a researcher, you will never know the truth (in this case, that the treatment effect is 5) and thus you are always estimating the ATE (or any other estimand). 
               But when you are simulating and omniscient, you will always be calculating, since you know the true treatment effect.'),
            br(),
            h3('Calculate the true SATE'),
            verbatimTextOutput('simulation_sate_code'),
            textOutput('simulation_sate'),
            h3('Use a difference in mean outcomes to estimate SATE.'),
            verbatimTextOutput('simulation_mean_diff_code'),
            textOutput('simulation_mean_diff'),
            h3('Use Linear Regression to estimate SATE'),
            verbatimTextOutput('simulation_reg_code'),
            textOutput('simulation_reg'),
             
             
#              p("Simulation of random variables is important in applied statistics for several reasons. 
# First, we use several probability models to mimic variation in the world, and the tools of simulation can help us better understand how this variation plays out.
#                Second, we can use simulation to approximate the sampling distribution of data and propagate this to the sampling distribution of statistical estimates and procedures.
#                Third, regression models are not deterministic; they produce probabilistic predictions. Simulation is the most convenient and general way to represent uncertainties in forecasts.")
),
    tabPanel("Estimator Comparisons",
             p('Now we will further explore the properties of these two different approaches to estimating our ATEs by simulation.
               For now we will only consider the variability in estimates that would manifest as a result of the randomness in who is assigned to receive the treatment (this is sometimes referred to as “randomization based inference”). 
               Since we are wearing omniscient hat we can see how the observed outcomes and estimates would change across a distribution of possible treatment assignments. 
               We simulate this by repeatedly drawing a new vector of treatment assignments and then for each new dataset calculating estimates using our two estimators above.'),
             verbatimTextOutput('mean_diff_reg_compare'),
             plotOutput('mean_diff_compare'),
             plotOutput('reg_compare'),
             verbatimTextOutput('mean_diff_biasedness_code'),
             textOutput('mean_diff_biasedness'),
             
             verbatimTextOutput('reg_biasedness_code'),
             textOutput('reg_biasedness'),
             verbatimTextOutput('mean_diff_efficiency_code'),
             textOutput('mean_diff_efficiency'),
             
             verbatimTextOutput('reg_efficiency_code'),
             textOutput('reg_efficiency')
          
             
             ), # use sampling distribution to compare unbiasedness and efficiency
    tabPanel("Exercise")
))

server <- function(input, output, session) {
  students <- c('James','Robert', 'John',
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
                'Nurul', 'Haruto', 'Ren', 'Akari', 'Salomé', 'Oliver', 'Aadya', 'Saanvi', 'Yinuo')
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
    table <- data.frame(Group = c(as.integer(1),as.integer(0)), Frequence = c(as.integer(df$treatment), as.integer(input$select_n_binomial - df$treatment)))
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
      sd <- paste0('Sample Sd: ', round(sd(as.numeric(tmp$score)),1))
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
  
  output$sampling_distr <- renderText(choose(100000,100))
  
  
  observeEvent(input$generate_sampling_distribution, {
    output$sampling_distribution_normal <- renderPlot({
      
      all_means <- data.frame(data = rep(NA, input$select_n_sampling_distribution))
      for (i in 1:input$select_n_sampling_distribution) {
        sample <- rnorm(n = 100, mean = input$select_mean_normal_sampling, sd = input$select_sd_normal_sampling)
        tmp <- mean(sample)
        all_means$data[i] <- tmp
      }
      ggplot() + geom_histogram(data = all_means, aes(x = data, y = ..density..), bins = 30, alpha = 0.5) +
        geom_vline(xintercept = mean(all_means$data), color = 'blue') 
      
    })
  })
  
  output$sampling_distribution_bernoulli <- renderPlot({
    proportions <- data.frame(data = rep(NA, input$select_n_bernoulli_sampling))
    for (i in 1:input$select_n_bernoulli_sampling) {
      tmp <- rbinom(n = 100, size = 1, prob = input$select_prob_bernoulli_sampling)
      proportions$data[i] <- mean(tmp)
    }
    ggplot() + geom_histogram(data = proportions, aes(x = data, y = ..density..), bins = 30, alpha = 0.5) +
      geom_vline(xintercept = mean(proportions$data), color = 'blue') 
  })
  
  
  # output$regression <- renderPlot({
  #   x <- seq(from = 1, to = 10, length.out = input$sample_size)
  #   y <- input$select_b0 + input$select_b1*x + rnorm(length(x), 0,input$select_sigma)
  #   df <- data.frame(x, y)
  #   ggplot(df, aes(x = x, y = y)) + geom_point() + geom_smooth(method='lm', formula= y~x,se = F) + theme_bw()
  # })
  
  Z <- rbinom(n = 100, size = 1, prob = 0.5)
  X <- rnorm(n = 100, mean = 65, sd = 3)
  tau <- 5
  Y0 <- 10 + 1.1*X + rnorm(100, mean = 0, sd = 1)
  Y1 <- 10 + 1.1*X + tau + rnorm(100, mean = 0, sd = 1)
  Y <- ifelse(Z == 1,  Y1, Y0)
  
  output$simulation_treatment_code <- renderText({
    "rbinom(n = 100, size = 1, prob = 0.5)"
  })
  
  output$simulation_treatment <- renderText({
    text <- c()
    for (i in 1:100) {
      text <- c(text, paste0(students[i], ': ', round(Z[i])))
    }
    toString(text)
  })
  
  output$simulation_prescore_code <- renderText({
    "rnorm(n = 100, mean = 60, sd = 10)"
  })
  
  output$simulation_prescore <- renderText({
    text <- c()
    for (i in 1:100) {
      text <- c(text, paste0(students[i], ': ', round(X[i])))
    }
    toString(text)
  })
  
  output$simulation_treatment <- renderText({
    text <- c()
    for (i in 1:100) {
      text <- c(text, paste0(students[i], ': ', round(Z[i])))
    }
    toString(text)
  })
  
  output$simulation_postscore_code <- renderText({
    "tau <- 5 \nY0 <- 10 + X + 0 + rnorm(100, mean = 0, sd = 1) \nY1 <- 10 + X + tau + rnorm(100, mean = 0, sd = 1) \nY <- ifelse(Z == 1,  Y1, Y0)"
  })
  
  output$simulation_postscore <- renderText({
    text <- c()
    for (i in 1:100) {
      text <- c(text, paste0(students[i], ': ', round(Y[i])))
    }
    toString(text)
  })
  
  
  output$simulation_sate_code <- renderText({
    "mean(Y1 - Y0)"
  })
  
  output$simulation_sate <- renderText({
    round(mean(Y1 - Y0), 2)
  })
  
  
  output$simulation_mean_diff_code <- renderText({
    "mean(Y[Z == 1]) - mean(Y[Z == 0])"
  })
  
  output$simulation_mean_diff <- renderText({
    round(mean(Y[Z == 1]) - mean(Y[Z == 0]), 2)
  })
  
  output$simulation_reg_code <- renderText({
    "fit <- lm(Y ~ X + Z) \nsummary(fit)$coefficients['Z', 1]"
  })
  
  output$simulation_reg <- renderText({
    fit <- lm(Y ~ X + Z)
    round(summary(fit)$coefficients['Z', 1], 2)
  })
  
  output$mean_diff_reg_compare <- renderText({
    "mean_diff <- c() \nlm_estimate <- c() \nN <- 100 \nfor (i in 1:10000) {\n    Z <- rbinom(N, 1, prob = 0.5) \n    Y <- ifelse(Z == 1, Y_1, Y_0)
    \n    mean_diff_tmp <- mean(Y[which(Z == 1)]) - mean(Y[which(Z == 0)]) \n    fit_tmp <- lm(Y ~ X + Z) \n    
    lm_estimate_tmp <- coef(fit_tmp)['Z'] \n    mean_diff <- c(mean_diff, mean_diff_tmp) \n    lm_estimate <- c(lm_estimate, lm_estimate_tmp)
}"
  })
  
  mean_diff <- c()
  lm_estimate <- c()
  N <- 100
  for (i in 1:10000) {
    Z <- rbinom(N, 1, prob = 0.5)
    Y <- ifelse(Z == 1, Y1, Y0)
    mean_diff_tmp <- mean(Y[which(Z == 1)]) - mean(Y[which(Z == 0)])
    fit_tmp <- lm(Y ~ X + Z)
    lm_estimate_tmp <- coef(fit_tmp)['Z']
    mean_diff <- c(mean_diff, mean_diff_tmp)
    lm_estimate <- c(lm_estimate, lm_estimate_tmp)
  }
  SATE <- mean(Y1 - Y0)
  
  output$mean_diff_compare <- renderPlot({
    
    hist(mean_diff, xlim = c(4,6), main = 'Distribution of Mean Difference')
    abline(v = SATE, col = 'red')
    abline(v = mean(mean_diff), col = 'blue')
    legend(5.5, 1700, c("SATE", "Mean"), col = c('red', 'blue'),
           lty = c(1, 1),bg = "gray90")
  })
  
  output$reg_compare <- renderPlot({
    
    hist(lm_estimate, xlim = c(4,6), main = 'Distribution of Regression Estimate')
    abline(v = SATE, col = 'red')
    abline(v = mean(mean_diff), col = 'blue')
    legend(5.5, 1700, c("SATE", "Mean"), col = c('red', 'blue'),
           lty = c(1, 1),bg = "gray90")
  })
  
  output$mean_diff_biasedness_code <- renderText({
    '(mean(mean_diff)-SATE)/sd(Y)'
  })
  output$mean_diff_biasedness <- renderText({
    (mean(mean_diff)-SATE)/sd(Y)
  })
  
  output$reg_biasedness_code <- renderText({
    '(mean(lm_estimate)-SATE)/sd(Y)'
  })
  output$reg_biasedness <- renderText({
    (mean(lm_estimate)-SATE)/sd(Y)
  })
  
  output$mean_diff_efficiency_code <- renderText({
    'sd(mean_diff)'
  })
  output$mean_diff_efficiency <- renderText({
    sd(mean_diff)
  })
  
  output$reg_efficiency_code <- renderText({
    'sd(lm_estimate)'
  })
  output$reg_efficiency <- renderText({
    sd(lm_estimate)
  })
  
  
  
}

shinyApp(ui, server)


