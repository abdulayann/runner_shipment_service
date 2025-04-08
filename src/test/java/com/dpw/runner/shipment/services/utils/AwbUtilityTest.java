package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.response.AwbShipmentInfoResponse;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingEventDto;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingStatusDto;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.impl.ShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.dto.response.AwbRoutingInfoResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AirMessagingStatus;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IGenericQueryRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.mockito.Mock;
import org.springframework.context.annotation.Lazy;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class AwbUtilityTest extends CommonMocks {

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
    @Mock
    private ShipmentSettingsDao shipmentSettingsDao;
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
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().build());

        testShipment = jsonTestUtility.getTestShipment();
        testConsol = jsonTestUtility.getJson("MAWB_CONSOLIDATION", ConsolidationDetails.class);

        testHawb = jsonTestUtility.getTestHawb();
        testDmawb = jsonTestUtility.getTestDmawb();
        testMawb = jsonTestUtility.getTestMawb();
    }




    @Test
    void testGetFormattedAddressBasic() {
        AwbAddressParam addressParam = new AwbAddressParam();
        addressParam.setAddress1("123 Main St");
        addressParam.setCity("City");
        addressParam.setCountry("Country");

        String expectedAddress = "123 Main St\r\nCity\r\nCountry";
        String formattedAddress = AwbUtility.getFormattedAddress(addressParam);

        assertEquals(expectedAddress, formattedAddress);
    }

    @Test
    void testGetFormattedAddressEmpty() {
        AwbAddressParam addressParam = new AwbAddressParam();
        String formattedAddress = AwbUtility.getFormattedAddress(addressParam);

        assertEquals("", formattedAddress);
    }

    @Test
    void testGetFormattedAddressComplete() {
        AwbAddressParam addressParam = new AwbAddressParam();
        addressParam.setAddress1("123 Main St");
        addressParam.setAddress2("Apt 101");
        addressParam.setCity("City");
        addressParam.setState("State");
        addressParam.setCountry("Country");
        addressParam.setPinCode("12345");
        addressParam.setContactNumber("123-456-7890");

        String expectedAddress = "123 Main St\r\nApt 101\r\nState\r\nCity\r\nCountry\r\n12345\r\n123-456-7890";
        String formattedAddress = AwbUtility.getFormattedAddress(addressParam);

        assertEquals(expectedAddress, formattedAddress);
    }

    @Test
    void testRoundOffAirShipment_NoChange() {
        double charge = 13.00;
        BigDecimal expected = new BigDecimal("13");
        BigDecimal result = AwbUtility.roundOffAirShipment(charge);
        assertEquals(expected, result);
    }

    @Test
    void testRoundOffAirShipment_NegativeValue() {
        double charge = -12.49;
        BigDecimal expected = BigDecimal.valueOf(-12);
        BigDecimal result = AwbUtility.roundOffAirShipment(charge);
        assertEquals(expected, result);
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsigner() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsignerOrgCode() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(new Parties());
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsignee() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingConsigneeOrgCode() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().build());
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingCarrierDetails() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().orgCode("org2").build());
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingCarrierDetailsShippingLine() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setConsigner(Parties.builder().orgCode("org1").build());
        shipmentDetails.setConsignee(Parties.builder().orgCode("org2").build());
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingOriginPort() {

        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails(null, "DestinationPort", 1L);
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingDestinationPort() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", null, 1L);
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingCarrierId() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", null);
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingMawbNumber() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", 1L);
        shipmentDetails.setJobType(ShipmentConstants.SHIPMENT_TYPE_DRT);
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_BlankMawbNumber() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", 1L);
        shipmentDetails.setJobType(ShipmentConstants.SHIPMENT_TYPE_DRT);
        shipmentDetails.setMasterBill("");
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
    }

    @Test
    void testValidateShipmentInfoBeforeGeneratingAwb_MissingHawbNumber() {
        ShipmentDetails shipmentDetails = createShipmentDetailsWithCarrierDetails("OriginPort", "DestinationPort", 1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setMasterBill("masterBill");
        assertThrows(ValidationException.class, () -> AwbUtility.validateShipmentInfoBeforeGeneratingAwb(shipmentDetails));
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
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingSendingAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingSendingAgentOrgCode() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(new Parties());
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingReceivingAgent() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org1").build()); // Ensure Sending Agent is not null
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingReceivingAgentOrgCode() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org1").build());
        consolidationDetails.setReceivingAgent(Parties.builder().orgCode("org1").build());
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingCarrierDetails() {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setSendingAgent(Parties.builder().orgCode("org2").build());
        consolidationDetails.setReceivingAgent(Parties.builder().orgCode("org1").build());
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingOriginPort() {
        ConsolidationDetails consolidationDetails = createConsolidationDetailsWithCarrierDetails(null, "DestinationPort");
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingDestinationPort() {
        ConsolidationDetails consolidationDetails = createConsolidationDetailsWithCarrierDetails("OriginPort", null);
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
    }

    @Test
    void testValidateConsolidationInfoBeforeGeneratingAwb_MissingMawbNumber() {
        ConsolidationDetails consolidationDetails = createConsolidationDetailsWithCarrierDetails("OriginPort", "DestinationPort");
        assertThrows(ValidationException.class, () -> AwbUtility.validateConsolidationInfoBeforeGeneratingAwb(consolidationDetails));
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


    @Test
    void createAirMessagingRequestForConsol() {
        Awb mockAwb = testMawb;
        mockAwb.setShipmentId(1L);
        ConsolidationDetails mockConsol = testConsol;


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setDefaultAddressId(2L);
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        AwbPaymentInfo awbPaymentInfo = new AwbPaymentInfo();
        awbPaymentInfo.setTotalPrepaid(BigDecimal.ZERO);
        awbPaymentInfo.setTotalCollect(BigDecimal.ZERO);
        awbAirMessagingResponse.setAwbPaymentInfo(awbPaymentInfo);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockConsol.getSendingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getSendingAgent().getOrgCode() + '#' + mockConsol.getSendingAgent().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockConsol.getReceivingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getReceivingAgent().getOrgCode() + '#' + mockConsol.getReceivingAgent().getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        List<EntityTransferOrganizations> orgsList = new ArrayList<>();
        orgsList.add(EntityTransferOrganizations.builder().build());
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(orgsList);
        when(v1Service.addressList(any())).thenReturn(new V1DataResponse());
        List<EntityTransferAddress> addressList = new ArrayList<>();
        addressList.add(EntityTransferAddress.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(addressList);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockConsol.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockConsol.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockShipmentSettings();
        mockTenantSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForConsole(mockAwb, mockConsol);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @Test
    void createAirMessagingRequestForConsol2() {
        Awb mockAwb = testMawb;
        mockAwb.setShipmentId(1L);
        ConsolidationDetails mockConsol = testConsol;


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setDefaultAddressId(2L);
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        awbAirMessagingResponse.setAwbPaymentInfo(null);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockConsol.getSendingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getSendingAgent().getOrgCode() + '#' + mockConsol.getSendingAgent().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockConsol.getReceivingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getReceivingAgent().getOrgCode() + '#' + mockConsol.getReceivingAgent().getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        List<EntityTransferOrganizations> orgsList = new ArrayList<>();
        orgsList.add(EntityTransferOrganizations.builder().build());
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(orgsList);
        when(v1Service.addressList(any())).thenReturn(new V1DataResponse());
        List<EntityTransferAddress> addressList = new ArrayList<>();
        addressList.add(EntityTransferAddress.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(addressList);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockConsol.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockConsol.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockShipmentSettings();
        mockTenantSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForConsole(mockAwb, mockConsol);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @Test
    void createAirMessagingRequestForConsol3() {
        Awb mockAwb = testMawb;
        mockAwb.setShipmentId(1L);
        ConsolidationDetails mockConsol = testConsol;


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setDefaultAddressId(2L);
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        AwbPaymentInfo awbPaymentInfo = new AwbPaymentInfo();
        awbPaymentInfo.setTotalPrepaid(null);
        awbPaymentInfo.setTotalCollect(BigDecimal.ZERO);
        awbAirMessagingResponse.setAwbPaymentInfo(awbPaymentInfo);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockConsol.getSendingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getSendingAgent().getOrgCode() + '#' + mockConsol.getSendingAgent().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockConsol.getReceivingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getReceivingAgent().getOrgCode() + '#' + mockConsol.getReceivingAgent().getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        List<EntityTransferOrganizations> orgsList = new ArrayList<>();
        orgsList.add(EntityTransferOrganizations.builder().build());
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(orgsList);
        when(v1Service.addressList(any())).thenReturn(new V1DataResponse());
        List<EntityTransferAddress> addressList = new ArrayList<>();
        addressList.add(EntityTransferAddress.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(addressList);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockConsol.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockConsol.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockShipmentSettings();
        mockTenantSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForConsole(mockAwb, mockConsol);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @Test
    void createAirMessagingRequestForConsol4() {
        Awb mockAwb = testMawb;
        mockAwb.setShipmentId(1L);
        ConsolidationDetails mockConsol = testConsol;


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setDefaultAddressId(2L);
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        AwbPaymentInfo awbPaymentInfo = new AwbPaymentInfo();
        awbPaymentInfo.setTotalPrepaid(BigDecimal.ZERO);
        awbPaymentInfo.setTotalCollect(null);
        awbAirMessagingResponse.setAwbPaymentInfo(awbPaymentInfo);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockConsol.getSendingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getSendingAgent().getOrgCode() + '#' + mockConsol.getSendingAgent().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockConsol.getReceivingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getReceivingAgent().getOrgCode() + '#' + mockConsol.getReceivingAgent().getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        List<EntityTransferOrganizations> orgsList = new ArrayList<>();
        orgsList.add(EntityTransferOrganizations.builder().build());
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(orgsList);
        when(v1Service.addressList(any())).thenReturn(new V1DataResponse());
        List<EntityTransferAddress> addressList = new ArrayList<>();
        addressList.add(EntityTransferAddress.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(addressList);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockConsol.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockConsol.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockShipmentSettings();
        mockTenantSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForConsole(mockAwb, mockConsol);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @Test
    void createAirMessagingRequestForConsol1() {
        Awb mockAwb = testMawb;
        mockAwb.setShipmentId(1L);
        ConsolidationDetails mockConsol = testConsol;


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setDefaultAddressId(2L);
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = new ArrayList<>();
        awbGoodsDescriptionInfoList.add(AwbGoodsDescriptionInfo.builder().rcp("XYZ").build());
        mockAwb.setAwbGoodsDescriptionInfo(awbGoodsDescriptionInfoList);

        List<AwbNotifyPartyInfo> awbNotifyPartyInfoList = new ArrayList<>();
        awbNotifyPartyInfoList.add(AwbNotifyPartyInfo.builder().specifiedAddressLocation("XYZ").build());
        mockAwb.setAwbNotifyPartyInfo(awbNotifyPartyInfoList);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        AwbPaymentInfo awbPaymentInfo = new AwbPaymentInfo();
        awbPaymentInfo.setTotalPrepaid(BigDecimal.ZERO);
        awbPaymentInfo.setTotalCollect(BigDecimal.ZERO);
        awbAirMessagingResponse.setAwbPaymentInfo(awbPaymentInfo);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockConsol.getSendingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getSendingAgent().getOrgCode() + '#' + mockConsol.getSendingAgent().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockConsol.getReceivingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getReceivingAgent().getOrgCode() + '#' + mockConsol.getReceivingAgent().getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        List<EntityTransferOrganizations> orgsList = new ArrayList<>();
        orgsList.add(EntityTransferOrganizations.builder().build());
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(orgsList);
        when(v1Service.addressList(any())).thenReturn(new V1DataResponse());
        List<EntityTransferAddress> addressList = new ArrayList<>();
        addressList.add(EntityTransferAddress.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(addressList);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockConsol.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockConsol.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        unlocationsMap.put("XYZ", mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockShipmentSettings();
        mockTenantSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForConsole(mockAwb, mockConsol);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @Test
    void createAirMessagingRequestForConsolWithAddressList() {
        Awb mockAwb = testMawb;
        mockAwb.setShipmentId(1L);
        ConsolidationDetails mockConsol = testConsol;

        Parties consolAddress = Parties.builder().type(Constants.FAG).orgCode("org1").addressCode("adCode").build();
        consolAddress.setOrgData(Map.ofEntries(
                Map.entry(PartiesConstants.COUNTRY, "test"),
                Map.entry(PartiesConstants.CITY, "test"),
                Map.entry(PartiesConstants.CURRENCY_CODE, "test"),
                Map.entry(PartiesConstants.KC_RA_EXPIRY, "test"),
                Map.entry(PartiesConstants.KC_RA_NUMBER, "test")
        ));
        mockConsol.setConsolidationAddresses(List.of(consolAddress));

        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        AwbPaymentInfo awbPaymentInfo = new AwbPaymentInfo();
        awbPaymentInfo.setTotalPrepaid(BigDecimal.ZERO);
        awbPaymentInfo.setTotalCollect(BigDecimal.ZERO);
        awbAirMessagingResponse.setAwbPaymentInfo(awbPaymentInfo);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockConsol.getSendingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getSendingAgent().getOrgCode() + '#' + mockConsol.getSendingAgent().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockConsol.getReceivingAgent().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockConsol.getReceivingAgent().getOrgCode() + '#' + mockConsol.getReceivingAgent().getAddressCode(), Collections.emptyMap());

        responseOrgs.put(consolAddress.getOrgCode(), Collections.emptyMap());
        responseAddrs.put(consolAddress.getOrgCode() + '#' + consolAddress.getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockConsol.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockConsol.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockShipmentSettings();
        mockTenantSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForConsole(mockAwb, mockConsol);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @ParameterizedTest
    @ValueSource(ints = {1,2,3})
    void createAirMessagingRequestForShipment(int args) throws RunnerException {
        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        ShipmentDetails mockShipment = testShipment;

        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setDefaultAddressId(2L);
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = new AwbAirMessagingResponse();

        AwbRoutingInfoResponse awbRoutingInfoResponse = new AwbRoutingInfoResponse();
        AwbPaymentInfo awbPaymentInfo = new AwbPaymentInfo();
        if(args == 1) awbPaymentInfo.setTotalPrepaid(BigDecimal.ZERO);
        if(args == 2) awbPaymentInfo.setTotalCollect(BigDecimal.ZERO);
        if(args == 3) {
            awbPaymentInfo.setTotalPrepaid(BigDecimal.ZERO);
            awbPaymentInfo.setTotalCollect(BigDecimal.ZERO);
        }

        OtherPartyInfo shipperPartyInfo = OtherPartyInfo.builder().additionalId("1").build();
        OtherPartyInfo consigneePartyInfo = OtherPartyInfo.builder().additionalId("2").build();
        OtherPartyInfo issuingAgentPartyInfo = OtherPartyInfo.builder().additionalId("3").build();
        OtherPartyInfo notifyPartyInfo = OtherPartyInfo.builder().additionalId("1").build();
        AwbShipmentInfoResponse awbShipmentInfoResponse = new AwbShipmentInfoResponse();
        awbShipmentInfoResponse.setShipperPartyInfo(shipperPartyInfo);
        awbShipmentInfoResponse.setConsigneePartyInfo(consigneePartyInfo);
        AwbNotifyPartyInfo awbNotifyPartyInfo = new AwbNotifyPartyInfo();
        if(args==3){
            awbShipmentInfoResponse.setIssuingAgentPartyInfo(issuingAgentPartyInfo);
            awbAirMessagingResponse.setAirMessagingAdditionalFields(AirMessagingAdditionalFields.builder().build());
            awbNotifyPartyInfo.setOtherPartyInfo(notifyPartyInfo);
        }
        awbAirMessagingResponse.setAwbPaymentInfo(awbPaymentInfo);
        awbAirMessagingResponse.setAwbRoutingInfo(List.of(awbRoutingInfoResponse));
        awbAirMessagingResponse.setAwbShipmentInfo(awbShipmentInfoResponse);
        awbAirMessagingResponse.setAwbNotifyPartyInfo(Collections.singletonList(awbNotifyPartyInfo));
        OCIInfo ociInfo = new OCIInfo();
        OtherIdentityInfo otherIdentityInfo = new OtherIdentityInfo();
        otherIdentityInfo.setIrIpAddress("127.0.0.1");
        ociInfo.setOtherIdentityInfo(otherIdentityInfo);
        awbAirMessagingResponse.setOciInfo(ociInfo);
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(mockShipment.getConsigner().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockShipment.getConsigner().getOrgCode() + '#' + mockShipment.getConsigner().getAddressCode(), Collections.emptyMap());
        responseOrgs.put(mockShipment.getConsignee().getOrgCode(), Collections.emptyMap());
        responseAddrs.put(mockShipment.getConsignee().getOrgCode() + '#' + mockShipment.getConsignee().getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                        .organizations(responseOrgs).addresses(responseAddrs).build();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(List.of(new ShipmentSettingsDetails()));
        List<EntityTransferOrganizations> orgsList = new ArrayList<>();
        orgsList.add(EntityTransferOrganizations.builder().build());
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(orgsList);
        when(v1Service.addressList(any())).thenReturn(new V1DataResponse());
        List<EntityTransferAddress> addressList = new ArrayList<>();
        addressList.add(EntityTransferAddress.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(addressList);

        var metaRoutingInfo = new AwbAirMessagingResponse.AwbRoutingInfoRes();
        String mockPort = "port";
        String mockByCarrier = "carrier";
        metaRoutingInfo.setOriginPortName(mockPort);
        metaRoutingInfo.setDestinationPortName(mockPort);
        metaRoutingInfo.setByCarrier("carrier");
        when(jsonHelper.convertValueToList(any(), eq(AwbAirMessagingResponse.AwbRoutingInfoRes.class))).thenReturn(
                List.of(metaRoutingInfo)
        );

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockShipment.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockShipment.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put(mockPort, mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);


        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        EntityTransferCarrier mockCarrier = new EntityTransferCarrier();
        carriersMap.put(mockByCarrier, mockCarrier);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        mockTenantSettings();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").isAwbRevampEnabled(true).build());
        mockShipmentSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForShipment(mockAwb, mockShipment, null, null);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    @Test
    void createAirMessagingRequestForShipmentWithShipAddress() throws RunnerException {
        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        ShipmentDetails mockShipment = testShipment;
        addShipmentDataForAwbGeneration(mockShipment);

        var issuingAgent = mockShipment.getShipmentAddresses().get(0);


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        // Arrange
        HttpServletRequest mockRequest = mock(HttpServletRequest.class);
        ServletRequestAttributes mockAttributes = mock(ServletRequestAttributes.class);

        try (MockedStatic<RequestContextHolder> mockedContextHolder = mockStatic(RequestContextHolder.class)) {
            // Setup request with expected IP
            mockedContextHolder.when(RequestContextHolder::getRequestAttributes)
                    .thenReturn(mockAttributes);
            when(mockAttributes.getRequest()).thenReturn(mockRequest);
            when(mockRequest.getHeader("X-Forwarded-For")).thenReturn("123.45.67.89");

            AwbAirMessagingResponse awbAirMessagingResponse = objectMapper.convertValue(mockAwb, AwbAirMessagingResponse.class);
            when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);
            when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(List.of());

            //Mock fetchOrgInfoFromV1
            HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
            HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

            responseOrgs.put(issuingAgent.getOrgCode(), Collections.emptyMap());
            responseAddrs.put(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode(), Collections.emptyMap());

            OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                    .organizations(responseOrgs).addresses(responseAddrs).build();
            when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);
            mockTenantSettings();
            mockShipmentSettings();
            var expectedResponse = awbUtility.createAirMessagingRequestForShipment(mockAwb, mockShipment, null, null);

            assertNotNull(expectedResponse);
            assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
            assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
        }
    }

    @Test
    void createAirMessagingRequestForShipmentWithShipAddress1() throws RunnerException {
        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        ShipmentDetails mockShipment = testShipment;
        addShipmentDataForAwbGeneration(mockShipment);
        Awb masterAwb = testMawb;

        List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = new ArrayList<>();
        awbGoodsDescriptionInfoList.add(AwbGoodsDescriptionInfo.builder().rcp("XYZ").build());
        mockAwb.setAwbGoodsDescriptionInfo(awbGoodsDescriptionInfoList);

        List<AwbNotifyPartyInfo> awbNotifyPartyInfoList = new ArrayList<>();
        awbNotifyPartyInfoList.add(AwbNotifyPartyInfo.builder().specifiedAddressLocation("XYZ").build());
        mockAwb.setAwbNotifyPartyInfo(awbNotifyPartyInfoList);
        OCIInfo ociInfo = new OCIInfo();
        OtherIdentityInfo otherIdentityInfo = new OtherIdentityInfo();
        otherIdentityInfo.setIrIpAddress("127.0.0.1");
        ociInfo.setOtherIdentityInfo(otherIdentityInfo);
        mockAwb.setOciInfo(ociInfo);

        var issuingAgent = mockShipment.getShipmentAddresses().get(0);


        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        mockTenantModel.setCountry("IND");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);


        AwbAirMessagingResponse awbAirMessagingResponse = objectMapper.convertValue(mockAwb, AwbAirMessagingResponse.class);
        when(jsonHelper.convertValue(any(), eq(AwbAirMessagingResponse.class))).thenReturn(awbAirMessagingResponse);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(List.of());

        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        UnlocationsResponse mockUnlocResponse = new UnlocationsResponse();
        unlocationsMap.put(mockShipment.getCarrierDetails().getOriginPort(), mockUnlocResponse);
        unlocationsMap.put(mockShipment.getCarrierDetails().getDestinationPort(), mockUnlocResponse);
        unlocationsMap.put("XYZ", mockUnlocResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);

        //Mock fetchOrgInfoFromV1
        HashMap<String, Map<String, Object>> responseOrgs = new HashMap<>();
        HashMap<String, Map<String, Object>> responseAddrs = new HashMap<>();

        responseOrgs.put(issuingAgent.getOrgCode(), Collections.emptyMap());
        responseAddrs.put(issuingAgent.getOrgCode() + '#' + issuingAgent.getAddressCode(), Collections.emptyMap());

        OrgAddressResponse mockOrgAddressResponse = OrgAddressResponse.builder()
                .organizations(responseOrgs).addresses(responseAddrs).build();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);
        mockTenantSettings();
        mockShipmentSettings();
        var expectedResponse = awbUtility.createAirMessagingRequestForShipment(mockAwb, mockShipment, null, masterAwb);

        assertNotNull(expectedResponse);
        assertEquals(2, expectedResponse.getMeta().getIssueingAgent().getCountry().length());
        assertEquals(2, expectedResponse.getMeta().getTenantInfo().getCountry().length());
    }

    private void addShipmentDataForAwbGeneration(ShipmentDetails shipment) {
        Parties shipmentAddress = Parties.builder().type(Constants.FAG).orgCode("org1").addressCode("adCode").build();
        shipmentAddress.setOrgData(Map.ofEntries(
                Map.entry(PartiesConstants.COUNTRY, "test"),
                Map.entry(PartiesConstants.CITY, "test"),
                Map.entry(PartiesConstants.CURRENCY_CODE, "test"),
                Map.entry(PartiesConstants.KC_RA_EXPIRY, "test"),
                Map.entry(PartiesConstants.KC_RA_NUMBER, "test")
        ));

        Routings routing = new Routings();
        routing.setLeg(1L);

        shipment.setShipmentAddresses(List.of(shipmentAddress));
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


    @ParameterizedTest
    @ValueSource(strings = {"INTERNAL_VALIDATION_ERROR", "EXTERNAL_VALIDATION_ERROR","INTERNAL_ERROR", "REJECTED", "INITIATED", "SUBMITTED", "RECEIVED"})
    void testCreateStatusUpdateForAirMessagingForMawb(String args) throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus(args);

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
    void testCreateStatusUpdateForAirMessagingForUnSupportedStatus() {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("randomStatusStringForTesting");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);

        assertThrows(RunnerException.class, () -> awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto));
    }

    @Test
    void testCreateStatusUpdateForAirMessagingForMawbSentHawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("RECEIVED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L).
                setAirMessageStatus(AwbStatus.AIR_MESSAGE_SENT));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingForMawbFailedHawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("RECEIVED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L).
                setAirMessageStatus(AwbStatus.AIR_MESSAGE_FAILED));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        var spyBean = Mockito.spy(awbUtility);
        doNothing().when(spyBean).sendAirMessagingFailureEmail(any(), any());

        spyBean.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingForMawbFailedHawbEmailThrowsException() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("RECEIVED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L).
                setAirMessageStatus(AwbStatus.AIR_MESSAGE_FAILED));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        var spyBean = Mockito.spy(awbUtility);
        doThrow(new IOException("test")).when(spyBean).sendAirMessagingFailureEmail(any(), any());

        spyBean.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingForMawbSuccessHawb() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus("RECEIVED");

        Awb mockAwb = testMawb;
        mockAwb.setTenantId(1);
        List<Awb> linkedHawb = List.of(testHawb.setShipmentId(1L).
                setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS));

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(linkedHawb);

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);


        verify(airMessagingLogsDao, times(1)).createAirMessagingLogs(any(), any(), any(), any(), any(), any(), any(), any());
        verify(eventDao, times(1)).createEventForAirMessagingStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingForHawbSentStatus() {
        var guid = UUID.randomUUID();
        String mockStatus = "SUBMITTED";
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus(mockStatus);

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        mockAwb.setTenantId(1);
        Awb mockMawb = testMawb;

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(testShipment));
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(List.of(
                mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SENT),
                mockMawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS)
        ));

        assertDoesNotThrow(()-> awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto));

    }

    @Test
    void testCreateStatusUpdateForAirMessagingForHawbFailedStatus(){
        var guid = UUID.randomUUID();
        String mockStatus = "SUBMITTED";
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus(mockStatus);

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setId(1L);
        mockShipment.setShipmentId("shipmentId");

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        mockAwb.setTenantId(1);
        mockAwb.setUserMailId("userEmail");
        Awb mockMawb = testMawb;
        mockMawb.setUserMailId("userEmail");

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(mockShipment));
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(List.of(
                mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_FAILED),
                mockMawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS)
        ));

        assertDoesNotThrow(()-> awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto));

    }

    @Test
    void testCreateStatusUpdateForAirMessagingForHawbFailedStatusEmailThrowsException() throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        String mockStatus = "SUBMITTED";
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus(mockStatus);

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setId(1L);
        mockShipment.setShipmentId("shipmentId");

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        mockAwb.setTenantId(1);
        mockAwb.setUserMailId("userEmail");
        Awb mockMawb = testMawb;
        mockMawb.setUserMailId("userEmail");

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(mockShipment));
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(List.of(
                mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_FAILED),
                mockMawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS)
        ));

        var spyBean = Mockito.spy(awbUtility);
        doThrow(new IOException("test")).when(spyBean).sendAirMessagingFailureEmail(any(), any());

        spyBean.createStatusUpdateForAirMessaging(airMessagingStatusDto);

        verify(spyBean, times(1)).sendAirMessagingFailureEmail(any(), any());
    }

    @Test
    void testCreateStatusUpdateForAirMessagingForHawbSuccessStatus() {
        var guid = UUID.randomUUID();
        String mockStatus = "SUBMITTED";
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus(mockStatus);

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        mockAwb.setTenantId(1);
        Awb mockMawb = testMawb;

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(testShipment));
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(List.of(
                mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS),
                mockMawb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS)
        ));

        assertDoesNotThrow(()-> awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto));

    }

    @ParameterizedTest
    @ValueSource(strings = {"SUBMITTED", "PROCESSED"})
    void testCreateStatusUpdateForAirMessagingForDmawb(String status) throws MessagingException, RunnerException, IOException {
        var guid = UUID.randomUUID();
        boolean isSuccess = true;
        AirMessagingStatusDto airMessagingStatusDto = new AirMessagingStatusDto();
        airMessagingStatusDto.setGuid(guid);
        airMessagingStatusDto.setStatus(status);

        Awb mockAwb = testDmawb;
        mockAwb.setShipmentId(1L);
        mockAwb.setTenantId(1);

        ShipmentDetails mockShipment = testShipment;
        mockShipment.setJobType(Constants.SHIPMENT_TYPE_DRT);

        when(awbDao.findAwbByGuidByQuery(guid)).thenReturn(mockAwb);
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(mockShipment));
        when(awbDao.findAllLinkedAwbs(guid)).thenReturn(List.of(
                mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SUCCESS)
        ));

        awbUtility.createStatusUpdateForAirMessaging(airMessagingStatusDto);
        assertTrue(isSuccess);
    }



    @Test
    void testCreateEventUpdateForAirMessagingThrowsException() {
        AirMessagingEventDto airMessagingEventDto = new AirMessagingEventDto();
        airMessagingEventDto.setGuid(UUID.randomUUID());


        when(awbDao.findByGuid(airMessagingEventDto.getGuid())).thenReturn(Optional.empty());
        assertThrows(RunnerException.class, ()-> awbUtility.createEventUpdateForAirMessaging(airMessagingEventDto));
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
    void testUpdateAwbStatusForFsuUpdateForAwbWithShipmentId() {
        AirMessagingEventDto airMessagingEventDto = new AirMessagingEventDto();
        airMessagingEventDto.setGuid(UUID.randomUUID());
        airMessagingEventDto.setEventCode(AwbConstants.FSU_LOCK_EVENT_CODE);

        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(1L);
        when(awbDao.findByGuid(airMessagingEventDto.getGuid())).thenReturn(Optional.of(mockAwb));

        try {
            awbUtility.createEventUpdateForAirMessaging(airMessagingEventDto);
            verify(awbDao, times(1)).updateAirMessageStatus(any(),any());
        }
        catch (Exception e){
            fail(e.getMessage());
        }
    }

    @Test
    void testUpdateAwbStatusForFsuUpdateForAwbWithConsolidationId() {
        AirMessagingEventDto airMessagingEventDto = new AirMessagingEventDto();
        airMessagingEventDto.setGuid(UUID.randomUUID());
        airMessagingEventDto.setEventCode(AwbConstants.FSU_LOCK_EVENT_CODE);

        Awb mockAwb = testMawb;
        AwbStatus mockStatus = AwbStatus.AWB_FSU_LOCKED;
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(1L).build();

        when(awbDao.findByGuid(airMessagingEventDto.getGuid())).thenReturn(Optional.of(mockAwb));
        when(consoleShipmentMappingDao.findByConsolidationIdByQuery(testMawb.getConsolidationId())).thenReturn(
            List.of(consoleShipmentMapping)
        );

        try {
            awbUtility.createEventUpdateForAirMessaging(airMessagingEventDto);
            verify(awbDao, times(1)).updateLinkedHawbAirMessageStatus(mockAwb.getGuid(), mockStatus.name());
            verify(awbDao, times(1)).updateAirMessageStatus(mockAwb.getGuid(), mockStatus.name());
        }
        catch (Exception e){
            fail(e.getMessage());
        }
    }


    @Test
    void testSendAirMessagingFailureEmail_AwbIsNull_NoExceptionThrown() {
        Awb awb = null;
        List<Awb> awbsList = new ArrayList<>();
        assertDoesNotThrow(() -> awbUtility.sendAirMessagingFailureEmail(awb, awbsList));
    }

    @Test
    void testSendAirMessagingFailureEmailMawbSuccessStatusLog() throws MessagingException, IOException {
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

        verify(emailServiceUtility, times(1)).sendEmail(any(), any(), any(), any(), any(), any());
    }

    @Test
    void testSendAirMessagingFailureEmailMawbFailureStatusLog() throws MessagingException, IOException {
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

        verify(emailServiceUtility, times(1)).sendEmail(any(), any(), any(), any(), any(), any());
    }

    @Test
    void testSendAirMessagingFailureEmailDmawbSuccessStatusLog() throws MessagingException, IOException {
        Awb mockAwb = testDmawb;
        mockAwb.setUserMailId("user email id");
        List<Awb> awbList = new ArrayList<>();

        AirMessagingLogs masterAirMessagingLogs = AirMessagingLogs.builder().status(AirMessagingStatus.FAILED.name()).build();

        when(shipmentDao.getShipmentNumberFromId(List.of(mockAwb.getShipmentId()))).thenReturn(
                List.of(testShipment));
        when(airMessagingLogsService.getRecentLogForEntityGuid(mockAwb.getGuid())).thenReturn(masterAirMessagingLogs);

        awbUtility.sendAirMessagingFailureEmail(mockAwb, awbList);

        verify(emailServiceUtility, times(1)).sendEmail(any(), any(), any(), any(), any(), any());
    }

    @Test
    void testOverrideInfoForCoLoadShipment() {

        var awbResponse = AwbAirMessagingResponse.builder()
                .meta(AwbAirMessagingResponse.Meta.builder()
                        .tenantInfo(AwbAirMessagingResponse.TenantInfo.builder().pimaAddress("1234").build())
                        .build())
                .build();
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.setPIMAAddress("98765");
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        awbUtility.overrideInfoForCoLoadShipment(awbResponse, true);

        assertEquals(mockTenantModel.getPIMAAddress(), awbResponse.getMeta().getTenantInfo().getPimaAddress());
    }

    @Test
    void testOverrideInfoForCoLoadShipment2() {

        var awbResponse = AwbAirMessagingResponse.builder()
                .meta(AwbAirMessagingResponse.Meta.builder()
                        .tenantInfo(AwbAirMessagingResponse.TenantInfo.builder().pimaAddress("1234").build())
                        .build())
                .build();


        awbUtility.overrideInfoForCoLoadShipment(awbResponse, false);

        assertEquals("1234", awbResponse.getMeta().getTenantInfo().getPimaAddress());
    }


}