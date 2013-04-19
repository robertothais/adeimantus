require File.expand_path("../support/test_client", __FILE__)

describe TestClient do

  subject { TestClient.new(:sender) }

  before do 
    subject.stub!(:send).and_return(true)
  end

  # The actual content of the stanzas isn't being checked.
  # This should be the case for thoroughness.

  describe "#send_stanza" do
    it "should send a test message" do
      subject.should_receive(:send).with(an_instance_of(Jabber::Message))
      subject.send_stanza
    end
  end

  describe "#come_online" do
    before { subject.stub(:connect) }
    after { subject.come_online }
    it "should connect" do
      subject.stub(:auth)
      subject.stub(:available)
      subject.should_receive(:connect)
    end
    it "should authenticate" do
      subject.stub(:connect)
      subject.stub(:available)
      subject.should_receive(:auth)
    end
    it "should make itself available" do
      subject.stub(:connect)
      subject.stub(:auth)
      subject.should_receive(:available)
    end
  end

  describe "#available" do
    it "should send a presence stanza" do
      subject.should_receive(:send).with(an_instance_of(Jabber::Presence))
      subject.available
    end
  end

  describe "#go_offline" do
    after { subject.go_offline }
    it "should make itself unavailable" do
      subject.should_receive(:send).with(an_instance_of(Jabber::Presence))
    end
    it "should close the connection" do
      subject.should_receive(:close)
    end
  end

  describe '#go_to_background' do    
    after { subject.go_to_background }
    it "should send a presence stanza"  do
      subject.should_receive(:send).with(an_instance_of(Jabber::Presence))
      
    end

    it 'should add a \'background\' child element to the stanza' do
      subject.should_receive(:send) do |stanza|
        stanza.elements['//background'].should_not be_nil
      end
    end
  end

  describe '#return_from_background' do
    it "should send a presence stanza"  do
      subject.should_receive(:send).with(an_instance_of(Jabber::Presence))
      subject.return_from_background 
    end
  end

  describe '#presence_stanza' do

    it 'should return a presence stanza' do
      subject.instance_eval { presence_stanza }.name.should eql("presence")
    end

    describe 'if we pass :unavailable as an argument' do
      it 'should set the type of the stanza to :unavailable' do
        stanza = subject.instance_eval { presence_stanza :unavailable }
        stanza.type.should eql(:unavailable)
      end
    end
  end
end