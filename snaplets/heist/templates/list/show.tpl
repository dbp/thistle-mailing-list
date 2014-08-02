<apply template="base">

  <h1><name/></h1>

  <a href="${addMemberPath}">Add member</a>
  <ul>
    <members>
      <li><email/> <is-subscribed><a href="${unsubscribePath}">unsubscribe</a></is-subscribed></li>
    </members>
  </ul>

</apply>
