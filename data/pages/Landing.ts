import * as React from 'react';
import * as Page from './Page';


class Landing extends React.Component {
  render() {
    return (
      <h1>this.props.text.shoutyHeader < /h1>
    )
  }
}

Landing.defaultProps = {
  text: {
    shoutyHeader: 'sup'
  }
}

export default Landing
