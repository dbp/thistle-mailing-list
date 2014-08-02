<apply template="base">

  <h1><name/></h1>

  <a href="${addMessagePath}">Add message</a>
  <ul>
    <messages>
      <li><a href="${editPath}"><subject/></a> <not-sent>Not sent</not-sent><is-sent>Sent</is-sent></li>
    </messages>
  </ul>

  <a href="${addMemberPath}">Add member</a>
  <ul>
    <members>
      <li><email/> <is-subscribed><a href="${unsubscribePath}">unsubscribe</a></is-subscribed></li>
    </members>
  </ul>

</apply>
