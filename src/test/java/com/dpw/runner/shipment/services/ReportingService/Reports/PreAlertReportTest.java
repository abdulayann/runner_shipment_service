package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.PreAlertModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.numberToWords;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PreAlertReportTest extends CommonMocks {

    @InjectMocks
    private PreAlertReport preAlertReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private IV1Service v1Service;

    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private Cache cache;

    @Mock
    private CustomKeyGenerator keyGenerator;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    private static ShipmentDetails shipmentDetails;

    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
    }

    @Test
    void populateDictionary() {
        PreAlertModel preAlertModel = new PreAlertModel();

        preAlertModel.setTenantDetails(new TenantModel());
        preAlertModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        preAlertModel.userdisplayname = UserContext.getUser().DisplayName;

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        preAlertModel.setShipmentContainers(Arrays.asList(shipmentContainers));

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("INR");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("INR");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setNoOfPacks(10);
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
        orgData.put(CITY, "123");
        orgData.put(STATE, "123");
        orgData.put(COUNTRY, "123");
        orgData.put(ZIP_POST_CODE, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        preAlertModel.setShipmentDetails(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));
        preAlertModel.setConsolidationDetails(consolidationModel);

        preAlertModel.noofpackages_word = numberToWords(preAlertModel.shipmentDetails.getNoOfPacks());

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(masterDataUtils.fetchMasterListFromCache(any())).thenReturn(new HashMap<>());


        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);

        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        locationMap.put(carrierDetailModel.getOrigin(), unlocationsResponse);
        locationMap.put(carrierDetailModel.getOriginPort(), unlocationsResponse);
        locationMap.put(carrierDetailModel.getDestinationPort(), unlocationsResponse);
        locationMap.put(carrierDetailModel.getDestination(), unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(locationMap);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(preAlertReport.populateDictionary(preAlertModel));
    }

    @Test
    void populateDictionaryWithDisplayPartyName() {
        PreAlertModel preAlertModel = new PreAlertModel();

        preAlertModel.setTenantDetails(new TenantModel());
        preAlertModel.setShipmentSettingsDetails(ShipmentSettingsDetails.builder().disableBlPartiesName(true).build());
        preAlertModel.userdisplayname = UserContext.getUser().DisplayName;

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        preAlertModel.setShipmentContainers(Arrays.asList(shipmentContainers));

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("INR");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("INR");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setNoOfPacks(10);
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        preAlertModel.setShipmentDetails(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));
        preAlertModel.setConsolidationDetails(consolidationModel);

        preAlertModel.noofpackages_word = numberToWords(preAlertModel.shipmentDetails.getNoOfPacks());

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(masterDataUtils.fetchMasterListFromCache(any())).thenReturn(new HashMap<>());

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(preAlertReport.populateDictionary(preAlertModel));
    }

    @Test
    void populateDictionaryWithoutConsolidation() {
        PreAlertModel preAlertModel = new PreAlertModel();

        preAlertModel.setTenantDetails(new TenantModel());
        preAlertModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        preAlertModel.userdisplayname = UserContext.getUser().DisplayName;

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        preAlertModel.setShipmentContainers(Arrays.asList(shipmentContainers));

        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.EXP);
        shipmentModel.setFreightLocal(BigDecimal.TEN);
        shipmentModel.setFreightLocalCurrency("INR");
        shipmentModel.setFreightOverseas(BigDecimal.TEN);
        shipmentModel.setFreightOverseasCurrency("INR");
        shipmentModel.setGoodsDescription("123");
        shipmentModel.setWeight(BigDecimal.TEN);
        shipmentModel.setVolume(BigDecimal.TEN);
        shipmentModel.setChargable(BigDecimal.TEN);
        shipmentModel.setVolumetricWeight(BigDecimal.TEN);
        shipmentModel.setNoOfPacks(10);
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ERN);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));

        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        preAlertModel.setShipmentDetails(shipmentModel);

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        preAlertModel.noofpackages_word = numberToWords(preAlertModel.shipmentDetails.getNoOfPacks());

        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>());

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(masterDataUtils.fetchMasterListFromCache(any())).thenReturn(new HashMap<>());

        v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setConsolidationId(1L);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put("id", "123");
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put("id", "123");
        when(jsonHelper.convertJsonToMap(any())).thenReturn(dictionary);
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(preAlertReport.populateDictionary(preAlertModel));
    }

    @ParameterizedTest
    @ValueSource(ints = {
            1000001,
            1001,
            101,
            21,
            10,
            -1
    })
    void getDocumentModel(int noOfPacks) {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setNoOfPacks(noOfPacks);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        mockShipmentSettings();
        assertNotNull(preAlertReport.getDocumentModel(123L));
    }
}
