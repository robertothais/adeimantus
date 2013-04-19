require 'timeout'
require 'xmpp4r'
require 'rexml/document'

class TestClient < Jabber::Client

  class << self
    attr_accessor :host, :port, :sender, :receiver, :use_ssl
  end

  CONNECT_TIMEOUT = 2
  DEFAULT_TIMEOUT = 2
  NAMESPACE = 'http://adeimantus.com'

  PASSWORD = '5c79ddd37'  

  # Creates test users on specified server
  def self.create_all! 
    errors = []   
    [ new(:sender), new(:receiver) ].each do |c|
      begin
        c.connect
        c.register(PASSWORD)
        c.close
      rescue => e 
        errors << e
      end
    end
    errors
  end

  def self.destroy_all!
    errors = []
    [ new(:sender), new(:receiver) ].each do |c|
      begin
        c.connect
        c.auth
        c.remove_registration
        c.close
      rescue => e 
        errors << e
      end
    end
    errors
  end

  def self.local!
    @host     = 'localhost'
    @port     = 5222  
    @sender   = 'testsender@localhost/adeimantus'
    @receiver = 'testreceiver@localhost/adeimantus'
    @use_ssl  = false
  end

  def self.staging!
    @host     = 'chat.adeimantus.com'
    @port     = 5222  
    @sender   = 'testsender@staging.adeimantus.com/adeimantus'
    @receiver = 'testreceiver@staging.adeimantus.com/adeimantus'
    @use_ssl  = true
  end

  def self.production!
    @host     = 'chat.adeimantus.com'
    @port     = 5222  
    @sender   = 'testsender@chat.adeimantus.com/adeimantus'
    @receiver = 'testreceiver@chat.adeimantus.com/adeimantus'
    @use_ssl  = true
  end

  def initialize(type)
    jid = case type
    when :sender then TestClient.sender
    when :receiver then TestClient.receiver
    end
    super(jid)
    @use_ssl = TestClient.use_ssl
  end

  def connect
    Timeout::timeout(CONNECT_TIMEOUT) do 
      super(TestClient.host, TestClient.port)
    end
  end

  def auth
    super(PASSWORD)
  end

  def come_online
    connect
    auth
    available
  end

  def go_offline
    send presence_stanza :unavailable
    close
  end

  def wait_for(timeout= DEFAULT_TIMEOUT, interval=0.5)
    Timeout::timeout(timeout) do
      until yield
        sleep(interval.to_f)
      end
    end
  end

  def available
    send presence_stanza
  end
  alias :return_from_background :available  

  def go_to_background
    presence_stanza = presence_stanza do |stanza|      
      background_element = REXML::Element.new("background")
      background_element.add_attribute('xmlns', NAMESPACE)
      stanza.add_element(background_element)
    end
    send presence_stanza
  end

  def send_stanza(receiver = TestClient.receiver)
    stanza = Jabber::Message.new(receiver, "Test")
    stanza.type = :chat
    send stanza
  end

  def send_stanzas(n, delay = 0.1)
    n.times do 
      send_stanza
      sleep delay
    end
  end

  private

    def presence_stanza(type = nil)
      Jabber::Presence.new.tap do |stanza|
        stanza.type = type unless type.nil?
        yield stanza if block_given?
      end
    end

  self.local!
end