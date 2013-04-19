require 'support/ejabberd_helper'
require 'support/test_client'
require 'support/test_server'

describe "The XMPP server" do

  before(:all) do
    exit unless Ejabberd.prepare
    @server = TestServer.new
    @server.start(5000)
  end

  after(:all) { @server.stop }

  before do
    TestClient.create_all!
  end

  after do
    TestClient.destroy_all!
  end

  let(:sender) do
    TestClient.new(:sender).tap do |c|
      c.come_online
    end
  end

  let(:receiver) do
    TestClient.new(:receiver).tap do |c|
      c.come_online
    end
  end

  shared_examples "the receiver is online" do
    it "should send an XMPP message" do 
      received = false
      receiver.add_message_callback { received = true }
      sender.send_stanza
      receiver.wait_for { received }
    end

    it "should not send a request to the push service" do
      wait_for_push = -> { @server.wait_for { sender.send_stanza } }
      expect(&wait_for_push).to raise_error Timeout::Error
    end
  end

  context "when the receiver is online" do
    before { receiver }
    include_examples "the receiver is online"
  end

  context "when the receiver is in the background" do
    before { receiver.go_to_background }
    it_behaves_like "the receiver is online"    
  end

  context "when the receiver is offline" do

    after do
      @server.wait_for { sender.send_stanza }
    end

    it "should call the /notify endpoint on the push service" do
      @server.on_request do |request|
        request.path.should eql('/notify')
      end      
    end

    context "when the receiver has no offline messages" do      
      it "should set the badge counter to the number of offline messages" do
        @server.on_request do |request| 
          request.params['badge'].should eql("1")
        end
      end
    end

    context "when the receiver has background messages" do
      it "should set the badge counter to the number of background plus offline messages" do
        receiver.go_to_background
        sender.send_stanzas 3
        receiver.go_offline
        @server.on_request do |request|          
          request.params['badge'].should eql("4")
        end
      end
    end

    context "when the receiver has had offline messages and came online before" do
      it "should set the badge counter to the new number of offline messages" do
        receiver.go_offline
        sender.send_stanzas 3
        receiver.come_online
        receiver.go_offline
        @server.on_request do |request| 
          request.params['badge'].should eql("1")
        end
      end
    end

    context "when the receiver has had background messages and came to the foreground before" do
      it "should set the badge counter to the new number of offline messages" do
        receiver.go_to_background
        sender.send_stanzas 3
        receiver.return_from_background
        receiver.go_offline
        @server.on_request do |request|
          request.params['badge'].should eql("1")
        end        
      end
    end

    context "when the receiver has had background messages, gone offline and came online before" do
      it "should set the badge counter to the new number of offline messages" do
        receiver.go_to_background
        sender.send_stanzas 3
        receiver.go_offline
        receiver.come_online  
        receiver.go_offline
        @server.on_request do |request|
          request.params['badge'].should eql("1")
        end
      end
    end
  end
end