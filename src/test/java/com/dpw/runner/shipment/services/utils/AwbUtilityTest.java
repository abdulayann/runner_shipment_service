package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.Kafka.Dto.AirMessagingEventDto;
import com.dpw.runner.shipment.services.Kafka.Dto.AirMessagingStatusDto;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbAddressParam;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AirMessagingStatus;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IGenericQueryRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.mockito.Mock;
import org.springframework.context.annotation.Lazy;

import javax.mail.MessagingException;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AwbUtilityTest {

    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ConsolidationDetails testConsol;
    private static ObjectMapper objectMapper;
    private static Awb testHawb;
    private static Awb testDmawb;
    private static Awb testMawb;
    @Mock
    public MasterDataUtils masterDataUtils;
    @Mock
    private IAirMessagingLogsDao airMessagingLogsDao;
    @Mock
    private IAirMessagingLogsService airMessagingLogsService;
    @Mock
    @Lazy
    private IAwbDao awbDao;
    @InjectMocks
    private AwbUtility awbUtility;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private EmailServiceUtility emailServiceUtility;
    @Mock
    private IEventDao eventDao;
    @Mock
    private IGenericQueryRepository genericQueryRepository;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IV1Service v1Service;
    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build());

        testShipment = jsonTestUtility.getTestShipment();
        testConsol = jsonTestUtility.getJson("MAWB_CONSOLIDATION", ConsolidationDetails.class);

        testHawb = jsonTestUtility.getTestHawb();
        testDmawb = jsonTestUtility.getTestDmawb();
        testMawb = jsonTestUtility.getTestMawb();
    }




    @Test
    public void testGetFormattedAddressBasic() {
        AwbAddressParam addressParam = new AwbAddressParam();
        addressParam.setAddress1("123 Main St");
        addressParam.setCity("City");
        addressParam.setCountry("Country");

        String expectedAddress = "123 Main St\r\nCity\r\nCountry";
        String formattedAddress = awbUtility.getFormattedAddress(addressParam);

        assertEquals(expectedAddress, formattedAddress);
    }

    @Test
    public void testGetFormattedAddressEmpty() {
        AwbAddressParam addressParam = new AwbAddressParam();
        String formattedAddress = awbUtility.getFormattedAddress(addressParam);

        assertEquals("", formattedAddress);
    }

    @Test
    public void testGetFormattedAddressComplete() {
        AwbAddressParam addressParam = new AwbAddressParam();
        addressParam.setAddress1("123 Main St");
        addressParam.setAddress2("Apt 101");
        addressParam.setCity("City");
        addressParam.setState("State");
        addressParam.setCountry("Country");
        addressParam.setPinCode("12345");
        addressParam.setContactNumber("123-456-7890");

        String expectedAddress = "123 Main St\r\nApt 101\r\nState\r\nCity\r\nCountry\r\n12345\r\n123-456-7890";
        String formattedAddress = awbUtility.getFormattedAddress(addressParam);

        assertEquals(expectedAddress, formattedAddress);
    }


    @Test
    public void testConstructAddressBasic() {
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(PartiesConstants.ADDRESS1, "123 Main St");
        addressData.put(PartiesConstants.CITY, "City");
        addressData.put(PartiesConstants.COUNTRY, "Country");

        String expectedAddress = "\r\n123 Main St\r\nCity\r\nCountry";
        String constructedAddress = awbUtility.constructAddress(addressData);

        assertEquals(expectedAddress, constructedAddress);
    }

    @Test
    public void testConstructAddressEmpty() {
        Map<String, Object> addressData = new HashMap<>();
        String constructedAddress = awbUtility.constructAddress(addressData);

        assertEquals("", constructedAddress);
    }

    @Test
    public void testConstructAddressNull() {
        String constructedAddress = awbUtility.constructAddress(null);

        assertEquals("", constructedAddress);
    }

    @Test
    public void testConstructAddressComplete() {
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(PartiesConstants.ADDRESS1, "123 Main St");
        addressData.put(PartiesConstants.ADDRESS2, "Apt 101");
        addressData.put(PartiesConstants.CITY, "City");
        addressData.put(PartiesConstants.STATE, "State");
        addressData.put(PartiesConstants.COUNTRY, "Country");
        addressData.put(PartiesConstants.ZIP_POST_CODE, "12345");
        addressData.put(PartiesConstants.CONTACT_PHONE, "123-456-7890");

        String expectedAddress = "\r\n123 Main St\r\nApt 101\r\nState\r\nCity\r\nCountry\r\n12345\r\n123-456-7890";
        String constructedAddress = awbUtility.constructAddress(addressData);

        assertEquals(expectedAddress, constructedAddress);
    }

    @Test
    public void testRoundOffAirShipment_NoChange() {
        double charge = 13.00;
        BigDecimal expected = new BigDecimal(13.00);
        BigDecimal result = awbUtility.roundOffAirShipment(charge);
        assertEquals(expected, result);
    }

    @Test
    public void testRoundOffAirShipment_NegativeValue() {
        double charge = -12.49;
        BigDecimal expected = new BigDecimal(-12.00);
        BigDecimal result = awbUtility.roundOffAirShipment(charge);
        assertEquals(expected, result);
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsigner() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsignerOrgCode() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(new Parties());
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsignee() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsigneeOrgCode() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().build());
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingCarrierDetails() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().orgCode("org2").build());
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingCarrierDetailsShippingLine() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().orgCode("org2").build());
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingOriginPort() {

        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails(null, "DestinationPort", 1L);
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingDestinationPort() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", null, 1L);
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingCarrierId() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", null);
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingMawbNumber() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", 1L);
        shipmentDetails.setJobType(ShipmentConstants.SHIPMENT_TYPE_DRT);
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_BlankMawbNumber() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", 1L);
        shipmentDetails.setJobType(ShipmentConstants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setMasterBill("");
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    public void testValidateShipmentInfoBeforeGeneratingAwb_MissingHawbNumber() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", 1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setMasterBill("masterBill");
        assertThrows(ValidationException.class, () -> awbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    private ShipmentDetails createShipmentDetailsWithCarrierDetails(String originPort, String destinationPort, Long carrierId) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().orgCode("org2").build());
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setShippingLine("shippingLine");
        carrierDetails.setOriginPort(originPort);
        carrierDetails.setDestinationPort(destinationPort);
        carrierDetails.setId(carrierId);
        shipmentDetails.setCarrierDetails(carrierDetails);
        return shipmentDetails;
    }


    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingSendingAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingSendingAgentOrgCode() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(new Parties());
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingReceivingAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org1").build()); // Ensure Sending Agent is not null
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingReceivingAgentOrgCode() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org1").build());
        consolidationDetails.setReceivingAgent(Parties.builder().orgCode("org1").build());
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingCarrierDetails() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org1").build());
        consolidationDetails.setReceivingAgent(Parties.builder().orgCode("org1").build());
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingOriginPort() {
        ConsolidationDetails consolidationDetails = createConsolidationDetailsWithCarrierDetails(null, "DestinationPort");
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingDestinationPort() {
        ConsolidationDetails consolidationDetails = createConsolidationDetailsWithCarrierDetails("OriginPort", null);
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    public void testValidateConsolidationInfoBeforeGeneratingAwb_MissingMawbNumber() {
        ConsolidationDetails consolidationDetails = createConsolidationDetailsWithCarrierDetails("OriginPort", "DestinationPort");
        assertThrows(ValidationException.class, () -> awbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    private ConsolidationDetails createConsolidationDetailsWithCarrierDetails(String originPort, String destinationPort) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org1").build());
        consolidationDetails.setReceivingAgent(Parties.builder().orgCode("org1").build());
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOriginPort(originPort);
        carrierDetails.setDestinationPort(destinationPort);
        consolidationDetails.setCarrierDetails(carrierDetails);
        return consolidationDetails;
    }




    // ************** Air Messaging ****************** //



    void createAirMessagingRequestForShipment() {
        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        ShipmentDetails mockShipment = testShipment;
//        addShipmentDataForAwbGeneration(mockShipment);


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);


        awbUtility.createAirMessagingRequestForShipment(mockAwb, mockShipment);

    }

    private void addShipmentDataForAwbGeneration(ShipmentDetails shipment) {
        Parties shipmentAddress = Parties.builder().type(Constants.FAG).orgCode("org1").addressCode("adCode").build();
        shipmentAddress.setOrgData(Map.ofEntries(
                Map.entry(ReportConstants.COUNTRY, "test"),
                Map.entry(ReportConstants.CITY, "test_city")
        ));

        Routings routing = new Routings();
        routing.setLeg(1L);

        shipment.setShipmentAddresses(List.of(shipmentAddress));
//        shipment.setRoutingsList(List.of(routing));
    }


    @Test
    void createStatusUpdateForAirMessagingThrowsExceptionForEmptyAwb() {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);

        Awb mockAwb = null;

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);

        assertThrows(RunnerException.class, () -> awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto));
    }



    @Test
    void testCreateStatusUpdateForAirMessagingFor_INTERNAL_VALIDATION_ERROR_Mawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("INTERNAL_VALIDATION_ERROR");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);
    }

    @Test
    void testCreateStatusUpdateForAirMessagingFor_REJECTED_Mawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("REJECTED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingFor_SUBMITTED_Mawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("SUBMITTED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingFor_RECEIVED_Mawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("RECEIVED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingFor_UNSUPPORTED_STATUS_Mawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("randomStatusStringForTesting");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);

        assertThrows(RunnerException.class, () -> awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto));
    }

    @Test
    void testCreateStatusUpdateForAirMessagingFor_RECEIVED_Hawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("RECEIVED");

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        mockAwb.setTenantId(1);

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(testShipment));
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(List.of(mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS)));

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);

    }



    @Test
    void testCreateEventUpdateForAirMessagingForMawb() {
        AirMessagingEventDto airMessagingEventDto = new AirMessagingEventDto();
        airMessagingEventDto.setGuid(UUID.randomUUID());

        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(1L).build();


        when(awbDao.findByGuid(airMessagingEventDto.getGuid())).thenReturn(Optional.of(testMawb));
        when(consoleShipmentMappingDao.findByConsolidationIdByQuery(testMawb.getConsolidationId())).thenReturn(
                List.of(consoleShipmentMapping)
        );

        try {
            awbUtility.createEventUpdateForAirMessaging(airMessagingEventDto);
        }
        catch (Exception e){
            fail(e.getMessage());
        }
    }

    @Test
    void testCreateEventUpdateForAirMessagingForHawb() {
        AirMessagingEventDto airMessagingEventDto = new AirMessagingEventDto();
        airMessagingEventDto.setGuid(UUID.randomUUID());

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        when(awbDao.findByGuid(airMessagingEventDto.getGuid())).thenReturn(Optional.of(mockAwb));

        try {
            awbUtility.createEventUpdateForAirMessaging(airMessagingEventDto);
        }
        catch (Exception e){
            fail(e.getMessage());
        }
    }


    @Test
    public void testSendAirMessagingFailureEmail_AwbIsNull_NoExceptionThrown() throws MessagingException, IOException {
        Awb awb = null;
        List<Awb> awbsList = new ArrayList<>();
        assertDoesNotThrow(() -> awbUtility.sendAirMessagingFailureEmail(awb, awbsList));
    }

    @Test
    public void testSendAirMessagingFailureEmailMawbSuccessStatusLog() throws MessagingException, IOException {
        Awb mockAwb = testMawb;
        mockAwb.setUserMailId("user email id");
        List<Awb> awbList = List.of(testHawb.setShipmentId(1L), testHawb.setShipmentId(1L));

        AirMessagingLogs masterAirMessagingLogs = AirMessagingLogs.builder().status(AirMessagingStatus.SUCCESS.name()).build();
        AirMessagingLogs hawbAirMessagingLogs = AirMessagingLogs.builder().status(AirMessagingStatus.SUCCESS.name()).build();


        when(consolidationDetailsDao.getConsolidationNumberFromId(mockAwb.getConsolidationId())).thenReturn(
                testConsol.getConsolidationNumber()
        );
        when(airMessagingLogsService.getRecentLogForEntityGuid(mockAwb.getGuid())).thenReturn(masterAirMessagingLogs);
        when(airMessagingLogsService.getRecentLogForEntityGuid(testHawb.getGuid())).thenReturn(hawbAirMessagingLogs);

        awbUtility.sendAirMessagingFailureEmail(mockAwb, awbList);

        verify(emailServiceUtility, times(1)).sendEmail(any(), any(), any(), any(), any());
    }

    @Test
    public void testSendAirMessagingFailureEmailMawbFailureStatusLog() throws MessagingException, IOException {
        Awb mockAwb = testMawb;
        mockAwb.setUserMailId("user email id");
        List<Awb> awbList = List.of(testHawb.setShipmentId(1L), testHawb.setShipmentId(1L));

        AirMessagingLogs masterAirMessagingLogs = AirMessagingLogs.builder().status(AirMessagingStatus.FAILED.name()).build();
        AirMessagingLogs hawbAirMessagingLogs = AirMessagingLogs.builder().status(AirMessagingStatus.FAILED.name()).build();

        when(consolidationDetailsDao.getConsolidationNumberFromId(mockAwb.getConsolidationId())).thenReturn(
                testConsol.getConsolidationNumber()
        );
        when(airMessagingLogsService.getRecentLogForEntityGuid(mockAwb.getGuid())).thenReturn(masterAirMessagingLogs);
        when(airMessagingLogsService.getRecentLogForEntityGuid(testHawb.getGuid())).thenReturn(hawbAirMessagingLogs);

        awbUtility.sendAirMessagingFailureEmail(mockAwb, awbList);

        verify(emailServiceUtility, times(1)).sendEmail(any(), any(), any(), any(), any());
    }

    @Test
    public void testSendAirMessagingFailureEmailDmawbSuccessStatusLog() throws MessagingException, IOException {
        Awb mockAwb = testDmawb;
        mockAwb.setUserMailId("user email id");
        List<Awb> awbList = new ArrayList<>();

        AirMessagingLogs masterAirMessagingLogs = AirMessagingLogs.builder().status(AirMessagingStatus.FAILED.name()).build();

        when(shipmentDao.getShipmentNumberFromId(List.of(mockAwb.getShipmentId()))).thenReturn(
                List.of(testShipment));
        when(airMessagingLogsService.getRecentLogForEntityGuid(mockAwb.getGuid())).thenReturn(masterAirMessagingLogs);

        awbUtility.sendAirMessagingFailureEmail(mockAwb, awbList);

        verify(emailServiceUtility, times(1)).sendEmail(any(), any(), any(), any(), any());
    }


}