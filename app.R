


library(shiny)
library(ggplot2)
theme_set(theme_bw())

ui <- fluidPage(
  
  titlePanel("Simulation"),
  
  navlistPanel(
    "Introduction",
    tabPanel('Get started!',
             p("Welcome to the simulation app! This app is intended for students who have some basic statistics knowledge and little to no knowledge of causal inference (R knowledge is optional). 
               Throughout your simulation journey, we will cover several different topics using a hypothetical real-world example that will hopefully build your intuition and knowledge about the joys of simulation. 
               One key thing to remember is that as you go through this app, you will have access to two very important hats: the _researcher_ hat, and the _omniscient_ hat. 
               The _researcher_ hat is one you wear daily - you are in the real world, and have normal human limitations. 
               However, every once in a while, you will get to wear the _omniscient_ hat, where you will transcend your feeble human mind and become an all-knowing and powerful being. 
               These two hats are very important when we are simulating, and will come into play quite often throughout our journey. Ready? Okay, let's get started.")),
    tabPanel("Hypothetical real-world example", 
      p('In New York State, there are a series of exams that high school students must take and pass in order to graduate. 
        These are known as the Regents exams, and they cover a wide variety of topics such as Algebra, Global History, English, and Earth Science.
        Generally, the Global History regents is considered to be one of the most difficult, primarily due to the sheer amount of memorization students need to do (it is a traumatizing memory for this author). 
        Students must score a 65 or higher to pass, but may re-take these exams as many times as necessary until they achieve their desired score. 
        However, these exams are generally offered once a year, so students wishing to re-take the exam must wait a year.'),
      p("Let's say you are a budding educational researcher who wants to understand how an afterschool pilot program would impact students' Global History regents scores. 
        However, you are but one researcher, and don't have the funding or a team to help collect the necessary data. 
        You also obviously don't have an actual afterschool pilot program either. But this is a question that has burned with the intensity of a thousand suns ever since you were in high school! 
        What if you had access to an afterschool program back then? Would you have done better on your Global History regents exam?"),
      p("Since this is a hypothetical example, we physically don't have any data. And as such, you can do one of two things to answer your research question: travel back in time, or simulate. 
         If you decide to go with option one, let me know - I have some scores I want settled with some people in my past. 
         More than likely however, you will need to _simulate your data_ to answer your question. Luckily, you have those two hats, which will open up worlds of possibilities, and be the key to solving this burning question of yours.")),
    tabPanel("What is simulation?",
             p("Simulation in broad terms, is a research or teaching technique that reproduces actual events and processes under test conditions. It is often used in industry, science, and education, and developing one is often a highly complex mathematical process. Initially a set of rules, relationships, and operating procedures are specified, along with other variables. The interaction of these phenomena create new situations, even new rules, which further evolve as the simulation proceeds. Simulation implementations range from paper-and-pencil and board-game reproductions of situations to complex computer-aided interactive systems."),
             p("A simulation imitates the operation of real world processes or systems with the use of models. The model represents the key behaviours and characteristics of the selected process or system, while the simulation represents how the model evolves under different conditions over time. Simulating your data also means you have the power to determine what the data are and how they get generated. All of this is called the _Data Generating Process_, and will be discussed in detail later in this app. "),
             p("Let's contextualize the broader definition of simulation to our hypothetical example, and put the _omniscient_ hat on, since it feels good to know everything. 
               With this powerful hat, you know that you have a sample of 100 fake students (that you will generate), who must be randomly assigned (by some model you will specify) into treatment and control groups. 
               'Treatment' in this case is the afterschool program, and thus those in the treatment group will go through the afterschool program, and those in the control group will receive normal tutoring. 
               You will also be generating all of these students' 'pre-treatment' test scores (again, through models), as well as their 'post-treatment' test scores, otherwise known as the _outcomes_. 
               These are fairly self-explanatory: **pre-treatment** test scores are the scores of the 100 sample students prior to any of them going through the afterschool program, and **post-treatment** test scores are the scores of the 100 sample students after the treatment period. 
               The great thing about being omniscient is that you will also be able to see what the treatment group students' scores are if they don't receive the treatment - these are called _potential outcomes_."),
             p("For comparison, let's switch to the _researcher_ hat for a moment to see the difference. As a mere researcher, you would still see the post-treatment scores for everyone, but you cannot know what the post-treatment test scores of the treatment group students would be _if they hadn't received the treatment_ (unless you can time travel, which again, let me know). The beauty of simulation is that it allows you to overcome this meta-physical roadblock to create a sort of 'parallel universe' where, everything else being exactly the same, students in the treatment group never received the treatment. This is key to making causal inference."),
             p("For now, understand that simulation starts all the way at the beginning: who is in your sample, and what are their pre-treatment test scores?"),
             p("This is an example of how a model (that you specify) would randomly select 50 students to be put into the treatment group. When you are randomly assigning students to receive the treatment, your model picks 50 students randomly. Thus, everytime your model runs, each simulation will result in a different group of 50 students. Try it for yourself!"),
             br(),
             p('Example: The possible combinations of students who would receive the treatment'),
             p('All 100 students: James, Robert, John, Michael, William, David, Richard, Joseph, Thomas, Charles, Christopher, Daniel, Matthew, Anthony, Mark, Donald, Steven, Paul, Andrew, Joshua, Kenneth, Kevin, Brian, George, Edward, Mary, Patricia, Jennifer, Linda, Elizabeth, Barbara, Yeri, Jessica, Sarah, Karen, Nancy, Lisa, Lee, Margaret, Sandra, Ashley, Kimberly, Emily, Donna, Michelle, Dorothy, Carol, Amanda, Melissa, Hee-jung'),
             
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
             p("You can think about any regular (deterministic) function like a box. For example, for the function $f(x) = x^2$, *every time* you give the function the value 2, it will *always* give you back 4. However, random variables are random functions that map the sample space (different students) into the real line (test score values between 0 and 100). 
               This means that each time you apply the function it will give you a different number.")),
    
    tabPanel("Discrete and continuous random variable",
             p('There are two main types of random variables: _discrete_ and _continuous_.'),
             p('*Discrete random variables* can only take on a countable number of values (possibly infinite, but oftentimes finite). 
               These are things like the number of Heads in two fair coin tosses, or the number of students assigned to a treatment condition (hint for later).'),
             p('Treatments randomly assigned to a student (i.e., a binary variable of 0 or 1) or the proportion of 1 in the treatments randomly assigned to 100 students.'),
             actionButton("one_student_treatment", "Assign a student"),
             textOutput('one_student_treatment_plot'),
             actionButton("hundred_student_treatment", "Assign 100 students"),
             plotOutput('hundred_students_treatment_plot'),
             br(),
             p('*Continuous random variables* can take on any real number, an uncountable amount of possibilities (i.e., to any amount of decimal places).'),
             p('Example: pre-treament score of a student randomly drawn, or the mean of pre-treament scores of 100 students randomly drawn'),
             actionButton("draw_a_student", "Draw a student"),
             textOutput('one_student_score'),
             actionButton("draw_hundred_student", "Draw 100 students"),
             plotOutput('hundred_students_scores')),
    
    tabPanel("Mean and standard deviation of a probability distribution",
             p('A probability distribution of a random variable $Z$ takes on some range of values (pre-treatment regents test scores between 0 and 100 of students from high schools in New York state). 
               The mean of this distribution is the average of all these scores or, equivalently, the score that would be obtained on average from a random sample from the distribution. 
               The mean is also called the expectation or expected value and is written as $E(Z)$ or $mu_Z$ . 
               For example, the plot below shows the (approximate) distribution of the pre-treatment regents test scores of students from high schools in New York state. 
               The mean of this distribution is 60: this is the average score of all the high school students in New York and it is also the average score we would expect to see from sampling one student at random.'),
             p('The variance of the distribution of $Z$ is $E((Z − mu_Z )^2)$, that is, the mean of the squared difference from the mean. 
               To understand this expression, first consider the special case in which $Z$ takes on only a single score In that case, this single value is the mean, 
               so $Z − mu_Z = 0$ for all $Z$ in the distribution, and the variance is 0. To the extent that the distribution has variation, 
               so that sampled scores from high school in New York State can be different, this will show up as values of $Z$ that are higher and lower than $mu_Z$, 
               and the variance of $Z$ is nonzero.'),
             p("The standard deviation is the square root of the variance. We typically work with the standard deviation rather than the variance because it is on the original scale of the distribution. 
               In the plot below, the standard deviation of students' scores is 10: that is, if you randomly sample a student from the population, observe their score z, and compute (z − 60)^2, then the average value you will get is 100; 
               this is the variance, and the standard deviation is $sqrt(100) = 10$."),
             actionButton("draw_sample", "Draw students"),
             plotOutput('mean_var')),
    tabPanel("Normal Distribution"),
    tabPanel("Binomial Distribution"),
    
    "Sampling Distribution",
    tabPanel("Component 5"),
    "Simulation",
    tabPanel("Data Generation Process (DGP)"),
    tabPanel("Average Treatment Effect (ATE)")
  )
)

server <- function(input, output, session) {
  students <- c('James','Robert', 'John',
                'Michael', 'William', 'David', 'Richard', 'Joseph', 'Thomas', 'Charles', 'Christopher', 'Daniel', 
                'Matthew', 'Anthony', 'Mark', 'Donald', 'Steven', 'Paul', 'Andrew', 'Joshua', 'Kenneth', 'Kevin', 
                'Brian', 'George', 'Edward', 'Mary', 'Patricia', 'Jennifer', 'Linda', 'Elizabeth', 'Barbara',
                'Yeri', 'Jessica', 'Sarah', 'Karen', 'Nancy', 'Lisa', 'Lee', 'Margaret', 'Sandra', 'Ashley',
                'Kimberly', 'Emily', 'Donna', 'Michelle', 'Dorothy', 'Carol', 'Amanda', 'Melissa', 'Hee-jung')
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
  
}

shinyApp(ui, server)


