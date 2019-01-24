/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 *
 * @format
 * @flow
 */

import React, {Component} from 'react';
import {Platform, StyleSheet, FlatList, Text, View, Alert, Button} from 'react-native';

const instructions = Platform.select({
  ios: 'Press Cmd+R to reload,\n' + 'Cmd+D or shake for dev menu',
  android:
    'Double tap R on your keyboard to reload,\n' +
    'Shake or press menu button for dev menu',
});

class TheList extends Component {
  constructor(props) {
    super(props);
    this.state = {
      items: [{key: 'a', text: "aaaa"},
              {key: 'b', text: "bbbb"}]
    };
  }

  add() {
    console.log(this);
    var newList = this.state.items.slice(0);
    const newItem = {key: "FACE", text: "FOO"};
    newList.unshift(newItem);
    this.setState({items: newList });
  }

  render() {
    return (
        <View>
        <Button
      onPress={()=> this.add()}
      title="press"
        />
        <FlatList
      style={styles.welcome}
      data={this.state.items}
      renderItem={({item}) => <Text>{item.text}</Text>}
        />
        </View>
    );
  }

}

export default class App extends Component {
  render() {
    return (
        <View style={styles.container}>
        <TheList/>
        </View>
    );
  }

}

const styles = StyleSheet.create({
  container: {
    marginTop: 30,
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF',
  },
  welcome: {
    fontSize: 20,
    textAlign: 'center',
    margin: 20,
  },
  instructions: {
    textAlign: 'center',
    color: '#333333',
    marginBottom: 5,
  },
});
