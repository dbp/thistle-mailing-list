<apply template="base">

  <dfForm method="post">
    <dfLabel ref="subject">Subject</dfLabel>
    <dfInputText ref="subject"/>
    <dfChildErrors ref="subject"/>
    <br/>
    <dfLabel ref="body">Body</dfLabel>
    <dfInputTextArea rows="20" cols="80" ref="body"/>
    <dfChildErrors ref="body"/>

    <dfInputSubmit/>
  </dfForm>

</apply>
