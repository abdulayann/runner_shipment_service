package com.dpw.runner.shipment.services.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.BDDMockito.willAnswer;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.anySet;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.UpdateOrgCreditLimitBookingResponse;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.DocumentDto;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;
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
    private MasterDataUtils masterDataUtils;

    @Mock
    private ICustomerBookingDao customerBookingDao;

    @Mock
    private IIntegrationResponseDao integrationResponseDao;

    @Mock
    private IShipmentService shipmentService;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IV1Service iv1Service;

    private static JsonTestUtility jsonTestUtility;

    @BeforeAll
    static void setup() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_FCL_CargoType() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @ParameterizedTest
    @MethodSource("provideCargoTypes")
    void testCreateBookingInPlatform(String cargoType) throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking(cargoType);
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
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

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_FCL_CargoType_ValidCarrier() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("FCL");

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
        when(masterDataUtils.fetchInBulkCarriers(anySet())).thenReturn(Map.of("Maersk Line", EntityTransferCarrier.builder().ItemValue("item val").Identifier1("code").build()));

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateBookingInPlatform_SuccessfulBooking_LCL_CargoType() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("LCL");
        customerBooking.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));

        bookingIntegrationsUtility.createBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
        verify(integrationResponseDao, times(1)).save(any());
    }

    @Test
    void testCreateBookingInPlatform_FailedBooking_throwsException() throws RunnerException {
        CustomerBooking customerBooking = getCustomerBooking("LCL");
        customerBooking.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));

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
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
        bookingIntegrationsUtility.updateBookingInPlatform(getCustomerBooking("FCL"));

        verify(platformServiceAdapter, times(1)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_TransportType_ROA() throws RunnerException {
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_ROA);
        bookingIntegrationsUtility.updateBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_TransportType_RAI() throws RunnerException {
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
        CustomerBooking customerBooking = getCustomerBooking("FCL");
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_RAI);
        bookingIntegrationsUtility.updateBookingInPlatform(customerBooking);

        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromCustomerBooking_throwsException() throws RunnerException {
        doThrow(new RuntimeException()).when(platformServiceAdapter).updateAtPlaform(any());
        when(masterDataUtils.getChargeTypes(anyList())).thenReturn(Map.of("ct1", EntityTransferChargeType.builder().Services("services").Description("Desc").build()));
        bookingIntegrationsUtility.updateBookingInPlatform(getCustomerBooking("FCL"));
        verify(platformServiceAdapter, times(1)).updateAtPlaform(any());
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_throwsException() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setBookingType(CustomerBookingConstants.ONLINE);
        shipment.setBookingReference("12345");
        bookingIntegrationsUtility.updateBookingInPlatform(shipment);
        verify(platformServiceAdapter, times((1))).createAtPlatform(any());
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_LCL_successfulUpdate() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setBookingType(CustomerBookingConstants.ONLINE);
        shipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipment.setBookingReference("1234");
        shipment.setPackingList(List.of(jsonTestUtility.getTestPacking()));
        shipment.setHouseBill("1234");
        shipment.setMasterBill("1234");
        Routings routings = new Routings();
        routings.setMode("SEA");
        routings.setPod("123");
        routings.setPol("234");
        shipment.setRoutingsList(List.of(routings));
        ReferenceNumbers referenceNumbers = new ReferenceNumbers();
        referenceNumbers.setCountryOfIssue("IND");
        referenceNumbers.setType("HBL");
        referenceNumbers.setReferenceNumber("1234");
        shipment.setReferenceNumbersList(List.of(referenceNumbers));
        bookingIntegrationsUtility.updateBookingInPlatform(shipment);
        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_LCL_TransportMode_ROA() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setBookingType(CustomerBookingConstants.ONLINE);
        shipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipment.setBookingReference("1234");
        shipment.setPackingList(List.of(jsonTestUtility.getTestPacking()));
        shipment.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        bookingIntegrationsUtility.updateBookingInPlatform(shipment);
        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_LCL_TransportMode_RAI() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setBookingType(CustomerBookingConstants.ONLINE);
        shipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipment.setBookingReference("1234");
        shipment.setPackingList(List.of(jsonTestUtility.getTestPacking()));
        shipment.setTransportMode(Constants.TRANSPORT_MODE_RAI);
        bookingIntegrationsUtility.updateBookingInPlatform(shipment);
        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_Offline() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setBookingType(CustomerBookingConstants.RUNNER);
        bookingIntegrationsUtility.updateBookingInPlatform(shipment);
        verify(platformServiceAdapter, times(0)).updateAtPlaform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_LCL_DifferentShipmentStatus_successfulUpdate() throws RunnerException {

        var bookedShipment = jsonTestUtility.getTestShipment();
        bookedShipment.setBookingType(CustomerBookingConstants.ONLINE);
        bookedShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        bookedShipment.setBookingReference("1234");
        bookedShipment.setStatus(1);
        bookedShipment.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        var cancelledShipment = jsonTestUtility.getTestShipment();
        cancelledShipment.setBookingType(CustomerBookingConstants.ONLINE);
        cancelledShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        cancelledShipment.setBookingReference("1234");
        cancelledShipment.setStatus(2);
        cancelledShipment.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        var confirmedShipment = jsonTestUtility.getTestShipment();
        confirmedShipment.setBookingType(CustomerBookingConstants.ONLINE);
        confirmedShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        confirmedShipment.setBookingReference("1234");
        confirmedShipment.setStatus(3);
        confirmedShipment.setPackingList(List.of(jsonTestUtility.getTestPacking()));

        bookingIntegrationsUtility.updateBookingInPlatform(bookedShipment);
        bookingIntegrationsUtility.updateBookingInPlatform(cancelledShipment);
        bookingIntegrationsUtility.updateBookingInPlatform(confirmedShipment);

        verify(platformServiceAdapter, times(3)).createAtPlatform(any(CommonRequestModel.class));
    }

    @Test
    void testUpdateBookingInPlatform_fromShipment_FCL_successfulUpdate() throws RunnerException {
        var shipment = jsonTestUtility.getTestShipment();
        shipment.setBookingType(CustomerBookingConstants.ONLINE);
        shipment.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipment.setBookingReference("1234");
        shipment.setContainersList(List.of(jsonTestUtility.getTestContainer()));
        bookingIntegrationsUtility.updateBookingInPlatform(shipment);
        verify(platformServiceAdapter, times(1)).createAtPlatform(any(CommonRequestModel.class));
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
        when(shipmentService.createShipmentInV2(any())).thenReturn((ResponseEntity.of(Optional.of(RunnerResponse.builder().build()))));
        CustomerBookingRequest customerBookingRequest = objectMapper.convertValue(customerBooking, CustomerBookingRequest.class);
        var response = bookingIntegrationsUtility.createShipmentInV2(customerBookingRequest);
        assertTrue(response.hasBody());
    }


    @Test
    void testCreateShipmentInV2_throwsException() throws RunnerException {
        willAnswer(invocation -> {
            throw new Exception("abc msg");
        }).given(shipmentService).createShipmentInV2(any());

        var customerBooking = getCustomerBooking("FCL");
        CustomerBookingRequest customerBookingRequest = objectMapper.convertValue(customerBooking, CustomerBookingRequest.class);

        assertThrows(Exception.class, () -> bookingIntegrationsUtility.createShipmentInV2(customerBookingRequest));
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

    @Test
    void testDocumentUploadEvent1() throws RunnerException {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_CREATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(true).entityType(Constants.SHIPMENTS_CAPS).entityId(entityId.toString()).build()
        ).build();

        var mockShipment = ShipmentDetails.builder().bookingType(CustomerBookingConstants.ONLINE).bookingReference(UUID.randomUUID().toString()).build();
        when(shipmentDao.findShipmentsByGuids(any())).thenReturn(List.of(mockShipment));
        when(platformServiceAdapter.updateAtPlaform(any())).thenReturn(ResponseHelper.buildSuccessResponse());

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(2)).findShipmentsByGuids(Set.of(entityId));

    }

    @Test
    void testDocumentUploadEvent2() throws RunnerException {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_CREATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(true).entityType(Constants.SHIPMENTS_CAPS).entityId(entityId.toString()).build()
        ).build();

        var mockShipment = ShipmentDetails.builder().bookingType(CustomerBookingConstants.ONLINE).bookingReference(UUID.randomUUID().toString()).build();
        when(shipmentDao.findShipmentsByGuids(any())).thenReturn(List.of(mockShipment));
        when(platformServiceAdapter.updateAtPlaform(any())).thenThrow(new RuntimeException("Simulated exception"));

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(2)).findShipmentsByGuids(Set.of(entityId));

    }

    @Test
    void testDocumentUploadEvent3() {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_CREATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(true).entityType(Constants.SHIPMENTS_CAPS).entityId(entityId.toString()).build()
        ).build();

        var mockShipment = ShipmentDetails.builder().bookingType(CustomerBookingConstants.ONLINE).build();
        when(shipmentDao.findShipmentsByGuids(any())).thenReturn(List.of(mockShipment));

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(2)).findShipmentsByGuids(Set.of(entityId));
    }

    @Test
    void testDocumentUploadEvent4() {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_CREATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(true).entityType(Constants.SHIPMENTS_CAPS).entityId(entityId.toString()).build()
        ).build();

        var mockShipment = ShipmentDetails.builder().build();
        when(shipmentDao.findShipmentsByGuids(any())).thenReturn(List.of(mockShipment));

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(2)).findShipmentsByGuids(Set.of(entityId));
    }

    @Test
    void testDocumentUploadEvent5() {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_CREATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(false).entityType(Constants.SHIPMENTS_CAPS).entityId(entityId.toString()).build()
        ).build();

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(1)).findShipmentsByGuids(Set.of(entityId));
    }

    @Test
    void testDocumentUploadEvent6() {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_CREATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(false).entityType(Constants.CONSOLIDATION).entityId(entityId.toString()).build()
        ).build();

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(0)).findShipmentsByGuids(Set.of(entityId));
    }

    @Test
    void testDocumentUploadEvent7() {
        var entityId = UUID.randomUUID();
        var documentDto = DocumentDto.builder().action(Constants.KAFKA_EVENT_UPDATE).data(
                DocumentDto.Document.builder().customerPortalVisibility(false).entityType(Constants.CONSOLIDATION).entityId(entityId.toString()).build()
        ).build();

        bookingIntegrationsUtility.documentUploadEvent(documentDto);

        verify(shipmentDao, times(0)).findShipmentsByGuids(Set.of(entityId));
    }
}