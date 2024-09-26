package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.ShipmentServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.NotesDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.UpdateOrgCreditLimitBookingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.masterDataObjects.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterDataObjects.dto.ChargeTypeMasterData;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.RetryCallback;
import org.springframework.retry.support.RetryTemplate;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class BookingIntegrationsUtilityTest {

    @InjectMocks
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Mock
    private IV1Service v1Service;

    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;

    @Mock
    private JsonHelper jsonHelper;

    private static ObjectMapper objectMapper;

    @Mock
    private IPlatformServiceAdapter platformServiceAdapter;

    @Mock
    private ShipmentServiceAdapter shipmentServiceAdapter;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private ICustomerBookingDao customerBookingDao;

    @Mock
    private IIntegrationResponseDao integrationResponseDao;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private NotesDao notesDao;

    @Mock
    private TenantSettingsService tenantSettingsService;

    @Mock
    private UserContext userContext;

    private static JsonTestUtility jsonTestUtility;

    @BeforeAll
    static void setup() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_FCL_CargoType() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @ParameterizedTest
    @MethodSource("provideCargoTypes")
    void testCreateBookingInPlatform(String cargoType) throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking(cargoType);
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_SEA);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
    }

    private static Stream<String> provideCargoTypes() {
        return Stream.of("FCL", "ROR", "BBK");
    }

    @Test
    void testCreateBookingInPlatform_ROA_TransportType() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");
//        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
//        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_ROA);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(0)).createAtPlatform(any(CommonRequestModel.class));
    }

    @Test
    void testCreateBookingInPlatform_RAI_TransportType() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");
//        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
//        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_RAI);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(0)).createAtPlatform(any(CommonRequestModel.class));
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_FCL_CargoType_EmptyCarrier() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        customerBooking.getCarrierDetails().setShippingLine("");

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_FCL_CargoType_ValidCarrier() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        when(masterDataUtils.fetchInBulkCarriers(anyList())).thenReturn(Map.of("Maersk Line", CarrierMasterData.builder().ItemValue("item val").Identifier1("code").build()));
        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_LCL_CargoType() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("LCL");
        customerBooking.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(1);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateBookingInPlatform_FailedBooking_throwsException() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("LCL");
        customerBooking.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        when(customerBookingDao.updateIsPlatformBookingCreated(anyLong(), eq(true))).thenReturn(0);

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateEmailIds() {
        var primaryEmail = "abc@xyz.com";
        var secondaryEmail = "dfs@abc.com";
        var result = bookingIntegrationsUtility.createEmailIds(primaryEmail, secondaryEmail);
        var result2 = bookingIntegrationsUtility.createEmailIds(null, secondaryEmail);
        var result3 = bookingIntegrationsUtility.createEmailIds(primaryEmail, null);
        assertNull(result2);
        assertNotNull(result);
        assertNotNull(result3);
        assertEquals(2, result.size());
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_succesfulUpdate() throws RunnerException {
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        bookingIntegrationsUtility.updateBookingInPlatform(getCustomerBooking("FCL"));

        verify(platformServiceAdapter, times(1)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_TransportType_ROA() throws RunnerException {
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_ROA);
        bookingIntegrationsUtility.updateBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_TransportType_RAI() throws RunnerException {
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_RAI);
        bookingIntegrationsUtility.updateBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_throwsException() throws RunnerException {
        doThrow(new RuntimeException()).when(platformServiceAdapter).updateAtPlaform(any());
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", ChargeTypeMasterData.builder().Services("services").Description("Desc").build()));
        bookingIntegrationsUtility.updateBookingInPlatform(getCustomerBooking("FCL"));
        verify(platformServiceAdapter, times(1)).updateAtPlaform(any());
    }

    @Test
    void createShipmentInV1_fromCustomerBooking_throwsException() throws RunnerException {
        willAnswer(invocation -> {
            throw new Exception("abc msg");
        }).given(v1Service).createBooking(any(CustomerBooking.class), anyBoolean(), anyBoolean(), any(UUID.class), any(HttpHeaders.class));
        assertThrows(Exception.class, () -> bookingIntegrationsUtility.createShipmentInV1(getCustomerBooking("FCL"), false, true, UUID.randomUUID(), HttpHeaders.EMPTY));
    }

    @Test
    void testCreateShipmentInV1_fromShipment_successfulUpdate() throws RunnerException {
        when(v1Service.createBooking(any(), anyBoolean(), anyBoolean(), any(UUID.class), any(HttpHeaders.class))).thenReturn(ResponseEntity.ok(null));
        bookingIntegrationsUtility.createShipmentInV1(getCustomerBooking("FCL"), false, true, UUID.randomUUID(), HttpHeaders.EMPTY);
        verify(v1Service, times(1)).createBooking(any(), anyBoolean(), anyBoolean(), any(UUID.class), any(HttpHeaders.class));
    }

    @Test
    void testCreateShipmentInV2_sucess() throws RunnerException {
        var customerBooking = getCustomerBooking("FCL");
        when(shipmentServiceAdapter.createShipment(any())).thenReturn(ShipmentDetailsResponse.builder().build());
        CustomerBookingRequest customerBookingRequest = objectMapper.convertValue(customerBooking, CustomerBookingRequest.class);
        var response = bookingIntegrationsUtility.createShipmentInV2(customerBookingRequest);
        assertNotNull(response);
    }


    @Test
    void testCreateShipmentInV2_throwsException() throws RunnerException {
        willAnswer(invocation -> {throw new Exception("abc msg");}).given(shipmentServiceAdapter).createShipment(any());

        var customerBooking = getCustomerBooking("FCL");
        when(notesDao.findByEntityIdAndEntityType(any(), anyString())).thenReturn(null);
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        CustomerBookingRequest customerBookingRequest = objectMapper.convertValue(customerBooking, CustomerBookingRequest.class);

        assertThrows(Exception.class, () -> bookingIntegrationsUtility.createShipmentInV2(customerBookingRequest));
    }

    @Test
    void testGetShipmentIdByGuid_success() throws RunnerException {
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        shipmentDetailsResponse.setId(1L);
        RunnerResponse<ShipmentDetailsResponse> runnerResponse = new RunnerResponse<>();
        runnerResponse.setData(shipmentDetailsResponse);
        when(shipmentServiceAdapter.getShipmentIdbyGuid(anyString())).thenReturn(ResponseEntity.ok(runnerResponse));
        ShipmentDetailsResponse response2 = bookingIntegrationsUtility.getShipmentIdByGuid("ancd");
        assertNotNull(response2.getId());
    }


    @Test
    void testGetShipmentIdByGuid_throwsException() throws RunnerException {
        willAnswer(invocation -> {
            throw new Exception("abc msg");
        }).given(shipmentServiceAdapter).getShipmentIdbyGuid(any());
        assertThrows(Exception.class, () -> bookingIntegrationsUtility.getShipmentIdByGuid("abcd"));
    }

    @Test
    void testUpdateOrgCreditLimitFromBooking_throwsException() throws RunnerException {
        willAnswer(invocation -> {
            throw new Exception("abc msg");
        }).given(v1Service).updateOrgCreditLimitFromBooking(any());
        assertThrows(Exception.class, () -> bookingIntegrationsUtility.updateOrgCreditLimitFromBooking(CheckCreditLimitResponse.builder().build()));
    }

    @Test
    void testUpdateOrgCreditLimitFromBooking_Success() throws RunnerException {
        when(v1Service.updateOrgCreditLimitFromBooking(any())).thenReturn(ResponseEntity.of(Optional.of(new UpdateOrgCreditLimitBookingResponse())));
        bookingIntegrationsUtility.updateOrgCreditLimitFromBooking(CheckCreditLimitResponse.builder().build());
        verify(v1Service, times(1)).updateOrgCreditLimitFromBooking(any());
    }

    @NotNull
    private CustomerBooking getCustomerBooking(String cargoType) {
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("BK123");
        customerBooking.setCarrierDetails(jsonTestUtility.getTestCarrierDetails());
        customerBooking.setCargoType(cargoType);
        customerBooking.setSalesBranch("branch1");
        customerBooking.setPrimarySalesAgentEmail("primary_agent");
        customerBooking.setSecondarySalesAgentEmail("secondary_agent");
        customerBooking.setContractId("123");
        customerBooking.setBookingStatus(BookingStatus.CANCELLED);
        customerBooking.setCustomer(Parties.builder().orgData(Map.of(PartiesConstants.FULLNAME, "org_name")).orgCode("org_code").build());

        Routings newRouting = new Routings();
        newRouting.setMode("mode");
        Routings newRouting2 = new Routings();

        customerBooking.setRoutingList(List.of(newRouting, newRouting2));
        Containers testContainer = jsonTestUtility.getTestContainer();
        customerBooking.setContainersList(List.of(testContainer));

        BookingCharges bookingCharges = new BookingCharges();
        bookingCharges.setContainersList(List.of(jsonTestUtility.getTestContainer()));
        bookingCharges.setChargeType("ct1");

        bookingCharges.setLocalSellAmount(BigDecimal.ONE);
        bookingCharges.setLocalSellCurrency("RS");
        bookingCharges.setOverseasSellAmount(BigDecimal.ONE);
        bookingCharges.setOverseasSellCurrency("RS");
        bookingCharges.setLocalCostCurrency("RS");
        bookingCharges.setSellExchange(BigDecimal.ONE);
        bookingCharges.setSellExchange(BigDecimal.TEN);
        bookingCharges.setGuid(UUID.randomUUID());

        customerBooking.setBookingCharges(List.of(bookingCharges));
        return customerBooking;
    }


    @Test
    void transformOrgAndAddressPayload_throwsException_NullData() {
        PartiesRequest request = new PartiesRequest();
        String addressCode = "AddressCode";
        String orgCode = "OrgCode";
        DependentServiceResponse v1OrgResponse = new DependentServiceResponse();
        var masterDataService = mock(IMasterDataService.class);
        when(masterDataFactory.getMasterDataService()).thenReturn(masterDataService);
        ArrayList<Parties> parties = new ArrayList<>();
        parties.add(Parties.builder().build());
        v1OrgResponse.setData(null);
        when(masterDataFactory.getMasterDataService().fetchOrganizationData(any())).thenReturn(v1OrgResponse);
        assertThrows(DataRetrievalFailureException.class, () -> bookingIntegrationsUtility.transformOrgAndAddressPayload(request, addressCode, orgCode));
    }

    @Test
    void transformOrgAndAddressPayload_SuccessfulTransformation() {
        PartiesRequest request = new PartiesRequest();
        String addressCode = "AddressCode";
        String orgCode = "OrgCode";
        DependentServiceResponse v1OrgResponse = new DependentServiceResponse();
        var masterDataService = mock(IMasterDataService.class);
        when(masterDataFactory.getMasterDataService()).thenReturn(masterDataService);
        ArrayList<Parties> parties = new ArrayList<>();
        parties.add(Parties.builder().build());
        v1OrgResponse.setData(parties);
        when(masterDataFactory.getMasterDataService().fetchOrganizationData(any())).thenReturn(v1OrgResponse);
        DependentServiceResponse v1AddressResponse = new DependentServiceResponse();
        v1AddressResponse.setData(parties);

        when(masterDataFactory.getMasterDataService().addressList(any())).thenReturn(v1AddressResponse);
        when(jsonHelper.convertToJson(any())).thenReturn("Json");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("Id", "1"));

        bookingIntegrationsUtility.transformOrgAndAddressPayload(request, addressCode, orgCode);

        assertNotNull(request.getOrgData());
        assertNotNull(request.getAddressData());
    }

    @Test
    void transformOrgAndAddressPayload_throwsDataRetrievalException() {
        PartiesRequest request = new PartiesRequest();
        String addressCode = "AddressCode";
        String orgCode = "OrgCode";
        DependentServiceResponse v1OrgResponse = new DependentServiceResponse();
        var masterDataService = mock(IMasterDataService.class);
        when(masterDataFactory.getMasterDataService()).thenReturn(masterDataService);
        ArrayList<Parties> parties = new ArrayList<>();
        parties.add(Parties.builder().build());
        v1OrgResponse.setData(parties);
        when(masterDataFactory.getMasterDataService().fetchOrganizationData(any())).thenReturn(v1OrgResponse);
        DependentServiceResponse v1AddressResponse = new DependentServiceResponse();

        ArrayList<Parties> emptyList = new ArrayList<>();
        v1AddressResponse.setData(emptyList);

        when(masterDataFactory.getMasterDataService().addressList(any())).thenReturn(v1AddressResponse);
        when(jsonHelper.convertToJson(any())).thenReturn("Json");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("Id", "1"));

        assertThrows(DataRetrievalFailureException.class, () -> bookingIntegrationsUtility.transformOrgAndAddressPayload(request, addressCode, orgCode));
    }

    @Test
    void createShipment_SuccessfulShipmentCreation2() throws Exception {
        // Mock data
        CustomerBooking customerBooking = new CustomerBooking();
        ShipmentDetailsResponse shipmentResponse = new ShipmentDetailsResponse();
        shipmentResponse.setGuid(UUID.randomUUID());

        // Mock retry template to execute without throwing an exception
        RetryTemplate retryTemplate = mock(RetryTemplate.class);
        lenient().doAnswer(invocation -> {
            RetryCallback<Object, Exception> callback = invocation.getArgument(0);
            return callback.doWithRetry(null);
        }).when(retryTemplate).execute(any());

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);

        // Call the method under test
        bookingIntegrationsUtility.createShipment(customerBooking, false, true, shipmentResponse, HttpHeaders.EMPTY);

        // Verify that createShipmentInV1 and updateBillStatus methods are called
//        verify(bookingIntegrationsUtility).createShipmentInV1(customerBooking, false, true, UUID.randomUUID(), HttpHeaders.EMPTY);
        verify(customerBookingDao).updateBillStatus(customerBooking.getId(), true);
    }

    @Test
    void createShipment_throwsException() throws Exception {
        // Mock data
        CustomerBooking customerBooking = new CustomerBooking();
        ShipmentDetailsResponse shipmentResponse = new ShipmentDetailsResponse();
        shipmentResponse.setGuid(UUID.randomUUID());

        // Mock retry template to throw an exception
        RetryTemplate retryTemplate = mock(RetryTemplate.class);
        lenient().doThrow(new RuntimeException("Simulated exception")).when(retryTemplate).execute(any());
        lenient().doThrow(new RuntimeException("Simulated exception")).when(v1Service).createBooking(any(), anyBoolean(), anyBoolean(), any(UUID.class), any(HttpHeaders.class));

        // Call the method under test
        assertThrows(RuntimeException.class, () -> {
            bookingIntegrationsUtility.createShipment(customerBooking, false, true, shipmentResponse, HttpHeaders.EMPTY);
        });

        // Verify that createShipmentInV1 and updateBillStatus methods are not called due to retry
//        verify(yourClass, never()).createShipmentInV1(any(), anyBoolean(), anyBoolean(), anyString(), any());
        verify(customerBookingDao, never()).updateBillStatus(anyLong(), anyBoolean());
    }
}