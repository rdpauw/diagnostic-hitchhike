## info-page
info_page <-
  tabItem(
    tabName = "info",
    fluidRow(
      column(
        HTML(
          "<h2>Hitchiker guide for diagnostic testing</h2>"
        ),
        HTML(
          "<p> Welcome to the online tutorial of the <b>Hitchiker guide for diagnostic testing</b>, which has
          specifically been developed for physiotherapists and/or manual therapists to support them
          in the process of <b>Evidence-Based Practice</b>.</p>"
        ),
        HTML(
          "<h2><i class='fa fa-question'></i>&nbsp; Anamnesis</h2>"
        ),
        HTML(
          "<p>In the <b>Anamnesis</b> tab, you can fill out different diagnoses under evaluation in the
          differential diagnosis. For each, you will have to provide a <b>Pre-test probability</b>: 
          If you are already convinced that a diagnosis will be confirmed (pre-test probability of 100%) 
          or exluded (pre-test probability of 100%), there is no need to go further in the process, 
          and select specific tests for your patient. However, if you are uncertain after the anamnesis 
          (pre-test probability between 20% and 80%), you can evaluate your patient with a clinical test. 
          For each of these tests, you can provide the test result: '?' for an unknown test result, 
          '+' for a positive test result, and '-' for a negative test result. <br><br>
      To estimate the initial pre-test probability, you can use the following guidelines:
      
      <ul>
      <li><b>Atypical anamnestic pattern</b>: pre-test probability = 30% [20-40%]</li>
      <li><b>Unsure</b>: pre-test probability = 50% [40-60%]</li>
      <li><b>Typical anamnestic pattern</b>: pre-test probability = 70% [60-80%]</li>
      </ul></p>"
          
        ),
        HTML(
          "<h2><i class='fa fa-fingerprint'></i>&nbsp; Find tests</h2>"),
        HTML(
          "<p>In the <b>Tests</b> tab, you can search for the most approriate clinical test and fill out
          its sensitivity and specificity. You will also have to provide the test result. 
          
          Hereafter, an overview is provided of the different estimates:
      
      <ul>
        <li><b>Prevalence</b>: the number of patients with the disorder in the study divided by the number of participants that were tested.</li>
        <li><b>Sensitivity</b>: the number of positive tests among participants with the disorder.</li>
        <li><b>Specificity</b>: the number of neagive tests among participants without the disorder.</li>
        <li><b>Likelihood ratios (LR)</b>: measures based on the combined sensitivity and specificity.</li>
      </ul></p>"
        ),
        HTML(
          "<h2><i class='fa fa-check'></i>&nbsp; Diagnosis</h2>"
        ),
        HTML(
          "<p>In the <b>Diagnosis</b> tab, you will find an overview of the results expressed a
          post-test probabilities.</p>"
        ),
        HTML(
          "<h2><i class='fa fa-plus'></i>&nbsp; Extra tools</h2>"
        ),
        HTML(
          "<p>Some extra tools such as a nomogram can be found here.</p>"
        ),
        width = 12
      )
    )
  )