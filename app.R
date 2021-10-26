
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
             p("This app is intended show how to simulate hypothetical samples in ordr to test if different causal inference methods are unbiased and efficient."),
             p("Throughout your simulation journey, we will cover several different topics using a hypothetical real-world example that will hopefully build your intuition and knowledge about the joys of simulation. 
               One key thing to remember is that as you go through this app, you will have access to two very important hats: the ", tags$em("researcher hat"), " and ", tags$em("omniscient hat"), ". 
               The",  tags$em("researcher hat"), " is one you wear daily - you are in the real world, and have normal human limitations. However, every once in a while, you will get to wear the ", tags$em("omniscient hat"), " where you will transcend your feeble human mind and become an all-knowing and powerful being. 
               These two hats are very important when we are simulating, and will come into play quite often throughout our journey."),
             p("Ready? Okay, let's get started.")),
    tabPanel("Hypothetical Example", 
      tags$div(
      p('In order to make the leanrning process easy and fun, we make up the following hypothetical example that linking concepts throughout the app.'),
      br(),
      p('In New York State, there are a series of exams that high school students must take and pass in order to graduate. 
        These are known as the Regents exams, and they cover a wide variety of topics such as Algebra, Global History, English, and Earth Science.
        Generally, the Global History regents is considered to be one of the most difficult, primarily due to the sheer amount of memorization students need to do (it is a traumatizing memory for this author). 
        Students must score a 65 or higher to pass, but may re-take these exams as many times as necessary until they achieve their desired score. 
        However, these exams are generally offered once a year, so students wishing to re-take the exam must wait a year.')),
      tags$div(
      p("Let's say you are a budding educational researcher who wants to understand how an afterschool pilot program would impact students' Global History regents scores. 
        However, you are but one researcher, and don't have the funding or a team to help collect the necessary data. 
        You also obviously don't have an actual afterschool pilot program either. But this is a question that has burned with the intensity of a thousand suns ever since you were in high school! 
        What if you had access to an afterschool program back then? Would you have done better on your Global History regents exam?")),
      tags$div(
      p("Since this is a hypothetical example, we physically don't have any data. And as such, you can do one of two things to answer your research question: travel back in time, or simulate. 
         If you decide to go with option one, let me know - I have some scores I want settled with some people in my past. 
         More than likely however, you will need to ", tags$strong("simulate your data"), " to answer your question. Luckily, you have those two hats, which will open up worlds of possibilities, and be the key to solving this burning question of yours."))),
    tabPanel("What is simulation?",
             p("Suppose the true causal effect of the afterschool program is 5 points higher in the Global History Regent score, but in reality we would never know the true causal effect value. 
               Instead what we can do is to use causal inference methods to estimate the effect of the afterschool program. However, due to lack of the true value, we don't know if our estimation can recover the true value or not.
               In order to test different causal inference methods valid or not, we can artificially generate data that simulate the real-world data and because we create all the data generation process we know the true causal effect in the simulation.
               With a correct answer, we now can examine if a causal inference method is valid or not when it is used to make inference on the truth."),
             p("Let's decompose the description of the hypothetical example, and put the ", tags$em("omniscient hat"), " on, since it feels good to know everything. 
               With this powerful hat, you know that you have a sample of 100 fake students (that you will generate), 
               who must be randomly assigned (by some model you will specify) into treatment and control groups. 
               'Treatment' in this case is the afterschool program, and thus those in the treatment group will go through the afterschool program, 
               and those in the control group will receive normal tutoring. You will also be generating all of these students' 'pre-treatment' test scores (again, through models), 
               as well as their 'post-treatment' test scores, otherwise known as the ", tags$em("outcome"), "."),
             p("These are fairly self-explanatory: ", tags$em("pre-treatment"), " test scores are the scores of the 100 sample students prior to any of them going through the afterschool program, and ", tags$em("post-treatment"), 
               " test scores are the scores of the 100 sample students after the treatment period. 
               The great thing about being omniscient is that you will also be able to see what the treatment group students' scores are if they don't receive the treatment - these are called", tags$em("potential outcomes"), "."),
             p("For comparison, let's switch to the ", tags$em("researcher hat"), " for a moment to see the difference. As a mere researcher, you would still see the post-treatment scores for everyone, 
               but you cannot know what the post-treatment test scores of the treatment group students would be ", tags$em("if they hadn't received the treatment"), " (unless you can time travel, which again, let me know). 
               The beauty of simulation is that it allows you to overcome this meta-physical roadblock to create a sort of 'parallel universe' where, 
               everything else being exactly the same, students in the treatment group never received the treatment. This is key to making causal inference."),
             p("For now, understand that simulation starts all the way at the beginning: who is in your sample, and what are their pre-treatment test scores?"),
             
             br(),
             h4('Example: Which students in your sample participate in the afterschool program?'),
             p("This is an example of how a model (that you specify) would randomly select 50 students to be put into the treatment group. 
               When you are randomly assigning students to receive the treatment, your model picks 50 students randomly. 
               Thus, everytime your model runs, each simulation will result in a different group of 50 students. Try it for yourself!"),
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
             p("Since the only limitation of simulation is computational, it can generate as many new results as you need, and represents the characterstics of the model(s) you defined. Now that you have a taste of what simulation looks like in its simplest form, let's take a short step back to really understand the underlying models and their distributional properties, since these really make up the core of your simulation study.")),
    
    
    "What is probability?",
    tabPanel("Random Variable and Probability Distributions",
             img(src = "population.png", height = 400, width = 700),
             p("Suppose you're drawing students with pre-treatment regents test scores between 0 and 100 from high schools in New York state. 
               When a student (let's call them Ming) is drawn at random, the corresponding “random variable” is Ming's pre-treatment score. 
               If we repeat this process for Ming, the probability of getting each possible score from the random variable is its distribution. 
               Distributions are about the population. In this example, the probability distribution of the random variable (pre-treatment test scores) specifies the probabilities of all possible outcomes, 
               such as the probability of a test score equaling 15, 65, 100, and so on."),
             p("You can think about any regular (deterministic) function like a box. For example, for the function f(x) = x^2, ", tags$em("every time"), " you give the function the value 2, it will ", tags$em("always"), " give you back 4. 
                However, random variables are random functions that map the sample space (different students) into the real line (test score values between 0 and 100). 
               This means that each time you apply the function it will give you a different number.")),
    
    tabPanel("Discrete and continuous random variable",
             p('There are two main types of random variables: ', tags$strong("discrete"), ' and ', tags$strong("continuous"), '.'),
             br(),
             p(tags$strong('Discrete random variables'), ' can only take on a countable number of values (possibly infinite, but oftentimes finite). 
               These are things like the number of Heads in two fair coin tosses, or the number of students assigned to a treatment condition (hint for later).'),
             p('In our example of afterschool program for the Global History regents exam, a discrete random variable is the treatments randomly assigned to a student, whhich can only take two values, treated or control. or the proportion of 1 in the treatments randomly assigned to 100 students.'),
             actionButton("one_student_treatment", "Assign a student"),
             textOutput('one_student_treatment_plot'),
             actionButton("hundred_student_treatment", "Assign 100 students"),
             plotOutput('hundred_students_treatment_plot'),
             br(),
             p(tags$strong('Continuous random variables'), ' can take on any real number, an uncountable amount of possibilities (i.e., to any amount of decimal places).'),
             p('Example: pre-treament score of a student randomly drawn, or the mean of pre-treament scores of 100 students randomly drawn'),
             actionButton("draw_a_student", "Draw a student"),
             textOutput('one_student_score'),
             actionButton("draw_hundred_student", "Draw 100 students"),
             plotOutput('hundred_students_scores')),
    
    tabPanel("Mean and Variance",
             p('A probability distribution of a random variable Z takes on some range of values (pre-treatment regents test scores between 0 and 100 of students from high schools in New York state). 
               The mean of this distribution is the average of all these scores or, equivalently, the score that would be obtained on average from a random sample from the distribution. 
               The mean is also called the expectation or expected value and is written as E(Z) or mu_Z. 
               For example, the plot below shows the (approximate) distribution of the pre-treatment regents test scores of students from high schools in New York state. 
               The mean of this distribution is 60: this is the average score of all the high school students in New York and it is also the average score we would expect to see from sampling one student at random.'),
             p('The variance of the distribution of Z is E((Z − mu_Z )^2), that is, the mean of the squared difference from the mean. 
               To understand this expression, first consider the special case in which Z takes on only a single score In that case, this single value is the mean, 
               so Z − mu_Z = 0 for all Z in the distribution, and the variance is 0. To the extent that the distribution has variation, 
               so that sampled scores from high school in New York State can be different, this will show up as values of Z that are higher and lower than mu_Z, 
               and the variance of Z is nonzero.'),
             p("The standard deviation is the square root of the variance. We typically work with the standard deviation rather than the variance because it is on the original scale of the distribution. 
               In the plot below, the standard deviation of students' scores is 10: that is, if you randomly sample a student from the population, observe their score z, and compute (z − 60)^2, then the average value you will get is 100; 
               this is the variance, and the standard deviation is sqrt(100) = 10."),
             actionButton("draw_sample", "Draw students"),
             plotOutput('mean_var')),
    tabPanel("Normal Distribution",
             p("Probably one of the most famous distributions in all of statistics is the _Normal distribution_. This is a distribution of a  continuous random variable, and is often used in simulation and teaching because it has very nice properties."),
             p('In addition to the distribution of data, probabilistic distributions are also commonly used in regression modeling to help us characterize the variation that remains after predicting the average --- 
               the error term epsilon in the expression y = a + bx + epsilon. These distributions allow us to get a handle on how uncertain our predictions are and, additionally, our uncertainty in the estimated parameters of the model.'),
             tags$div(
               sliderInput('normal_size', label = "Sampel Size", min = 10, max = 5000, value = 50, step = 1),
               numericInput(inputId = "select_normal_mean", "Mean:", 0),
               numericInput(inputId = "select_normal_sd", "Standard Deviation:", 1, max = 100)
             ),
             plotOutput('normal'),
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
             br()),
    tabPanel("Binomial Distribution",
             p("We've discussed discrete random variables as taking on a countable number of values. A famous distribution of discrete random variables that is often used as models for data is called the _Binomial distribution_. 
               This distribution has two parameters, n and p, where n is the number is 'trials', and p is the probability of 'success'. "),
             p("For example, let's say we flip a fair coin 100 times ('trials'), and count the number of Heads ('success'). 
               The random variable X is the number of Heads we observe, and the distribution of this random variable (Binomial) is shown here as an example."),
             tags$div(
               sliderInput('binom_size', label = "Sample Size", min = 10, max = 5000, value = 50, step = 1),
               numericInput(inputId = "select_binom_size", "number of trials:", value = 10, min = 1),
               numericInput(inputId = "select_binom_prob", "probability of success on each trial:", value = 0.5, min = 0, max = 1, step = 0.1)),
             plotOutput('binomial'),
             tags$div(
               useShinyjs(),
               actionButton("show_code_binomial", "Show me the R code of generating the distribution"),
               hidden(
                 div(id='code_div_binomial',
                     verbatimTextOutput("code_binomial")
                 )
               )
             ),
             p("Now let's take this idea and apply it to our example. 
               After we have generated the sample of 100 students and their respective test scores, 
               we need a way to randomly assign 50 of them to the treatment group and 50 of them to the control group. The binomial distribution provides an excellent model to generate this 'treatment assignment'. ")),
    
    "Sampling Distribution",
    tabPanel("What is Sampling Distribution?",
             p("The sampling distribution is the set of possible datasets that could have been observed if the data collection process had been re-done, along with the probabilities of these possible values."),
             p("The simplest example of a sampling distribution is the pure random sampling model: if the data are a simple random sample of size n from a population of size N, then the sampling distribution is the set of all samples of size n, all with equal probabilities."),
             p("The normal distribution, binomial distribution, and poisson distribution with specified parameters on the previous page are all sampling distributions for the samples of sizes of your choice."),
             p("Say there are 10,000 NYU grad students and we randomly select 1000 students. Here we have population size of 10,000 and sample size of 1000. How many samples of size “n” are possible out of a population of size “N”? That's 10000 choose 1000, ${1000 choose100}$, and the number is so large that even R only returns Inf."),
             code("choose(10000,1000)"),
             verbatimTextOutput('sampling_distr'),
             p("The next simplest example is pure measurement error: if observations $y_i$, i = 1,. . . , n, are generated from the model $y_i = a + bx_i + epsilon_i$ , with fixed coefficients a and b, pre-specified values of the predictor $x_i$ , and a specified distribution for the errors $epsilon_i$ 
               (for example, normal with mean 0 and standard deviation $sigma$), then the sampling distribution is the set of possible datasets obtained from these values of $x_i$ , drawing new errors $epsilon_i$ from their assigned distribution."),
             br(),
             p("One continuous predictor: y = b0 + b1x + eps"),
             numericInput(inputId = "select_b0", "Intercept (b0):", 1),
             numericInput(inputId = "select_b1", "Coefficient on X (b1):", 0.5),
             numericInput(inputId = "select_sigma", "Residual Std Dev (sigma):", 1),
             sliderInput(inputId = "sample_size",label = "Select Sample Size",
                         min = 10, max = 1000, value = 250, step = 10),
             plotOutput('regression'),
             p("In practice, we will not know the sampling distribution; we can only estimate it, as it depends on aspects of the population, not merely on the observed data. In the pure random sampling model, the sampling distribution depends on all N datapoints. For the measurement-error model, 
               the sampling distribution depends on the parameters a, b, and $sigma$, which in general are not known, and will be estimated from the data.")),
    "Simulation",
    tabPanel("Why simulation?",
             p("Simulation of random variables is important in applied statistics for several reasons. 
First, we use several probability models to mimic variation in the world, and the tools of simulation can help us better understand how this variation plays out.
               Second, we can use simulation to approximate the sampling distribution of data and propagate this to the sampling distribution of statistical estimates and procedures.
               Third, regression models are not deterministic; they produce probabilistic predictions. Simulation is the most convenient and general way to represent uncertainties in forecasts."),
             p("In this final section, we will use what we learned and simulated in the previous sections to answer the question you were initially tasked with at the beginning: Is the afterschool program effective in improving high school students' scores on the Global History regents exam? Remember that _omniscient_ hat? It's time to put it on."),
             p("As with any simulation study, we need to first establish our **Data Generating Process (DGP)**. This means explicitly stating how you will be generating all of the data you need to estimate the treatment effect later on. For the purposes of this study, we will use what we learned in previous sections to walk through our DGP.")),
    tabPanel("Data Generation Process (DGP)",
             h3('Treatment Assignment'),
             p("We already know how to generate treatment assignments from [section 2] using the Binomial distribution. The probability of assignment will be _0.5_."),
             h3("Generating pre-treatment test scores"),
             p("We also know that we can use the Normal distribution from [section 2] to generate our pre-treatment test scores. Remember: these are the original test scores of all the students prior to any of them attending the afterschool program."),
             h3('Generating outcome test scores based on treatment assignment'),
             p("Here we start to make good use of our omniscient hat. As omniscient beings, we know that the treatment effect (or \tau) is **5**. That is, we know that the post-treatment test scores of students who went through the afterschool program is on average **5** points higher than the students who did not. To generate these outcome scores, we would simulate a _dependency_ based on the treatment assignment variable from above:"),
             h3('Calculating the Average Treatment Effect (ATE)'),
             p('Note that we use "calculating" instead of "estimating". This is intentional, and is meant to illustrate the difference in process when you are wearing the omniscient hat versus the researcher hat. Specifically, as a researcher, you are always _estimating_ the ATE (or any other estimand) because we will never know the truth (in this case, that the treatment effect is 5). But when you are simulating and omniscient, you will always be calculating, since you know the true treatment effect.'),
             br()
             ),
    tabPanel("Average Treatment Effect (ATE)",
             p('Once we have simulated all the data necessary from our DGP, we can finally move on to calculating our estimand of interest:'))
  )
)

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
    studentlist <- sample(students, size = 25)
    output$student_list <- renderText(paste0(studentlist))
  })
  
  observeEvent(input$one_student_treatment, {
    treatment <- rbinom(1,size = 1, prob = 0.5)
    student <- sample(students, size = 1)
    output$one_student_treatment_plot <- renderText(paste0(student, ': ', treatment))
  })
  
  df <- reactiveValues(treatment = c(0,0,0))
  
  observeEvent(input$hundred_student_treatment, {
    df$treatment <- rbinom(100, size = 1, prob = 0.5)
    output$hundred_students_treatment_plot <- renderPlot({
      tmp <- data.frame(treatment = df$treatment)
      ggplot() + geom_histogram(data = tmp, aes(x = treatment, y = ..density..), bins = 30, alpha = 0.5) 
    })
  })
  
  observeEvent(input$draw_a_student, {
    score <- round(rnorm(1,60,15),2)
    student <- sample(students, size = 1)
    output$one_student_score <- renderText(paste0(student, ': ', score))
  })
  
  observeEvent(input$draw_hundred_student, {
    df$score <- rnorm(100,60,10)
    output$hundred_students_scores <- renderPlot({
      tmp <- data.frame(score = df$score)
      ggplot() + geom_histogram(data = tmp, aes(x = score, y = ..density..), bins = 30, alpha = 0.5) + 
        geom_vline(xintercept = mean(tmp$score), color = 'blue') 
    })
  })
  
  
  df1 <- reactiveValues(data = c())
  
  observeEvent(input$draw_sample, {
    df1$data <- rnorm(10000,60,10)
    output$mean_var <- renderPlot({
      tmp <- data.frame(data = df1$data)
      mean <- paste0('Mean: ', round(mean(tmp$data),1))
      sd <- paste0('Standard Deviation: ', round(sd(tmp$data),1))
      ggplot() + geom_histogram(data = tmp, aes(x = data, y = ..density..), 
                                bins = 30, alpha = 0.5) + 
        geom_vline(xintercept = mean(tmp$data)) +
        annotate("text",x=90,y=0.035,label=as.character(mean), fontface = "italic", size = 6) +
        annotate("text",x=90,y=0.032,label=as.character(sd), fontface = "italic", size = 6) 
    })
  })
  
  output$normal <- renderPlot({
    tmp <- data.frame(data = rnorm(n = input$normal_size, mean = input$select_normal_mean, sd = input$select_normal_sd))
    ggplot() + geom_histogram(data = tmp, aes(x = data, y = ..density..), bins = 30, alpha = 0.5)
  })
  observeEvent(input$show_code_normal, {
    toggle('code_div_normal')
    output$code_normal <- renderText({
      paste0('rnorm(n = ', input$normal_size, ', mean = ', input$select_normal_mean, ', sd = ', input$select_normal_sd, ')')
    })
    
  })
  
  output$binomial <- renderPlot({
    tmp <- data.frame(data = rbinom(n = input$binom_size, size = input$select_binom_size, prob = input$select_binom_prob))
    ggplot() + geom_histogram(data = tmp, aes(x = data, y = ..density..), bins = 30, alpha = 0.5)
  })
  observeEvent(input$show_code_binomial, {
    toggle('code_div_binomial')
    output$code_binomial <- renderText({
      paste0('rbinom(n = ', input$binom_size, ', size = ', input$select_binom_size, ', porb = ', input$select_binom_prob, ')')
    })
    
  })
  
  output$sampling_distr <- renderText(choose(10000,1000))
  
  output$regression <- renderPlot({
    x <- seq(from = 1, to = 10, length.out = input$sample_size)
    y <- input$select_b0 + input$select_b1*x + rnorm(length(x), 0,input$select_sigma)
    df <- data.frame(x, y)
    ggplot(df, aes(x = x, y = y)) + geom_point() + geom_smooth(method='lm', formula= y~x,se = F) + theme_bw()
  })
  
}

shinyApp(ui, server)


