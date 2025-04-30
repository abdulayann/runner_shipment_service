package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.DeliveryOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.impl.NPMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.AddressTranslationListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DeliveryOrderReportTest extends CommonMocks {

    @InjectMocks
    private DeliveryOrderReport deliveryOrderReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;
    @Mock
    private BillingServiceAdapter billingServiceAdapter;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private IHblDao hblDao;

    @Mock
    private NPMServiceAdapter npmServiceAdapter;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private Cache cache;

    @Mock
    private CustomKeyGenerator keyGenerator;

    private static final String LOC_CODE = "TEST";
    private static final String ORG_CODE = "ORG_TEST";
    private static final String ADDRESS_CODE = "ADDRESS_TEST";


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        mockUser.setLanguageCode("EN");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).build());
    }


    private static ShipmentDetails shipmentDetails;
    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).build());
    }

    private void mockVessel() {
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());
    }

    private void populateModel(DeliveryOrderModel deliveryOrderModel) {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(123L);
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
        shipmentModel.setSecurityStatus("Test");
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setPacksUnit("PKG");
        shipmentModel.setHouseBill("hsnn1234");

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setOrgCode(ORG_CODE);
        partiesModel.setAddressCode(ADDRESS_CODE);
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
        orgData.put(CITY, "123");
        orgData.put(STATE, "123");
        orgData.put(COUNTRY, "123");
        orgData.put(ZIP_POST_CODE, "123");
        orgData.put(ADDRESS1, "123");
        orgData.put(ADDRESS2, "123");
        orgData.put(ORG_FULL_NAME, "123");
        orgData.put(EMAIL, "123");
        orgData.put(CONTACT_PHONE, "123");


        orgData.put(PartiesConstants.RAW_DATA, "Text");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);

        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin(LOC_CODE);
        carrierDetailModel.setOriginPort(LOC_CODE);
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setDestinationPort(LOC_CODE);
        carrierDetailModel.setDestination(LOC_CODE);
        carrierDetailModel.setShippingLine("MAERSK");

        AdditionalDetailModel additionalDetailModel = new AdditionalDetailModel();
        additionalDetailModel.setPaidPlace("test");
        additionalDetailModel.setNotifyParty(partiesModel);
        additionalDetailModel.setDateOfIssue(LocalDateTime.now());
        additionalDetailModel.setDateOfReceipt(LocalDateTime.now());
        additionalDetailModel.setOnBoard("SHP");
        additionalDetailModel.setOnBoardDate(LocalDateTime.now());
        additionalDetailModel.setExportBroker(partiesModel);
        additionalDetailModel.setImportBroker(partiesModel);
        additionalDetailModel.setScreeningStatus(Arrays.asList(Constants.AOM));
        additionalDetailModel.setExemptionCodes("Test");
        additionalDetailModel.setAomFreeText("Test");
        additionalDetailModel.setGoodsCO("IND");
        additionalDetailModel.setBLChargesDisplay("PPD");
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        shipmentModel.setServiceType("Test");

        List<ShipmentContainers> shipmentContainersList = new ArrayList<>();
        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentContainersList.add(shipmentContainers);

        shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("40GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentContainersList.add(shipmentContainers);

        shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("40GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentContainersList.add(shipmentContainers);

        shipmentModel.setShipmentContainersList(shipmentContainersList);
        deliveryOrderModel.setContainers(shipmentModel.getShipmentContainersList());

        List<ContainerModel> containerModelList = new ArrayList<>();
        ContainerModel containers = new ContainerModel();
        containers.setContainerCount(1L);
        containers.setContainerCode("20GP");
        containers.setNetWeight(BigDecimal.TEN);
        containers.setContainerNumber("CONT000283");
        containers.setGrossVolume(BigDecimal.TEN);
        containers.setGrossVolumeUnit("M3");
        containers.setGrossWeight(BigDecimal.TEN);
        containers.setGrossWeightUnit("KG");
        containers.setPacksType("PKG");
        containers.setPacks("100");
        containerModelList.add(containers);
        shipmentModel.setContainersList(containerModelList);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        deliveryOrderModel.setShipmentDetails(shipmentModel);

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        List<PackingModel> packingModels = new ArrayList<>();
        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setPacks("10");
        packingModels.add(packingModel);

        PackingModel packingModel2 = new PackingModel();
        packingModel2.setLength(BigDecimal.TEN);
        packingModel2.setWidth(BigDecimal.TEN);
        packingModel2.setHeight(BigDecimal.TEN);
        packingModel2.setPacks("20");
        packingModels.add(packingModel2);
        shipmentModel.setPackingList(packingModels);

        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(INVNO);
        shipmentModel.setReferenceNumbersList(Arrays.asList(referenceNumbersModel));

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        ArrivalDepartureDetailsModel arrivalDepartureDetailsModel = new ArrivalDepartureDetailsModel();
        arrivalDepartureDetailsModel.setCTOId(partiesModel);
        arrivalDepartureDetailsModel.setContainerYardId(partiesModel);
        arrivalDepartureDetailsModel.setLastForeignPort("123");
        consolidationModel.setArrivalDetails(arrivalDepartureDetailsModel);
        consolidationModel.setDepartureDetails(arrivalDepartureDetailsModel);
        consolidationModel.setCreditor(partiesModel);
        consolidationModel.setAllocations(new AllocationsModel());
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        deliveryOrderModel.setConsolidationDetails(consolidationModel);
    }

    private Hbl populateHbl(){
        Hbl hbl = new Hbl();
        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setCargoGrossVolumeUnit("M3");
        hblDataDto.setCargoGrossWeightUnit("KG");
        hblDataDto.setCargoDescription("Test");
        hblDataDto.setHouseBill("Test");
        hblDataDto.setPlaceOfReceipt("Test");
        hblDataDto.setPackageCount(10);
        hbl.setHblData(hblDataDto);
        hbl.setHblNotifyParty(Arrays.asList(new HblPartyDto()));
        return hbl;
    }

    private void mockRakc(ShipmentModel shipmentModel) {
        Parties parties = new Parties();
        parties.setOrgCode("Test");
        parties.setAddressCode("Test");
        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> addressDataMap = new HashMap<>();
        addressDataMap.put(REGULATED_AGENT, true);
        addressDataMap.put(KCRA_NUMBER, ONE);
        addressDataMap.put(KCRA_EXPIRY, LocalDateTime.now());
        addressMap.put(parties.getOrgCode()+"#"+parties.getAddressCode(), addressDataMap);

        Parties parties2 = new Parties();
        parties2.setOrgCode("Test2");
        parties2.setAddressCode("Test2");
        addressDataMap = new HashMap<>();
        addressDataMap.put(KNOWN_CONSIGNOR, true);
        addressDataMap.put(KCRA_NUMBER, TWO);
        addressDataMap.put(KCRA_EXPIRY, LocalDateTime.now());
        addressMap.put(parties2.getOrgCode()+"#"+parties2.getAddressCode(), addressDataMap);

        OrgAddressResponse orgAddressResponse = new OrgAddressResponse();
        orgAddressResponse.setAddresses(addressMap);

        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);

        AddressTranslationListResponse addressTranslationListResponse = new AddressTranslationListResponse();
        AddressTranslationListResponse.AddressTranslationResponse addressTranslationResponse =  new AddressTranslationListResponse.AddressTranslationResponse();
        addressTranslationResponse.setOrgCode(ORG_CODE);
        addressTranslationResponse.setAddressCode(ADDRESS_CODE);
        addressTranslationListResponse.setAddressTranslationList(Arrays.asList(addressTranslationResponse));
        when(v1Service.getAddressTranslation(any())).thenReturn(addressTranslationListResponse);

    }

    @Test
    void populateDictionary_BillingIntegrationDisabled() throws RunnerException {
        DeliveryOrderModel deliveryOrderModel = new DeliveryOrderModel();
        deliveryOrderModel.setUsersDto(UserContext.getUser());
        deliveryOrderModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        populateModel(deliveryOrderModel);
        deliveryOrderModel.setTenantModel(new TenantModel());
        deliveryOrderModel.setHbl(populateHbl());
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(deliveryOrderModel.shipmentDetails);
        mockBill(false);
        mockUnloc();
        mockUnlocation();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(deliveryOrderReport.populateDictionary(deliveryOrderModel));
    }

    @Test
    void populateDictionary_BillingIntegrationEnabled() throws RunnerException {
        DeliveryOrderModel deliveryOrderModel = new DeliveryOrderModel();
        deliveryOrderModel.setUsersDto(UserContext.getUser());
        deliveryOrderModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        populateModel(deliveryOrderModel);
        deliveryOrderModel.setTenantModel(new TenantModel());
        deliveryOrderModel.setHbl(populateHbl());
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(deliveryOrderModel.shipmentDetails);
        mockBill(true);
        mockUnloc();
        mockUnlocation();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(deliveryOrderReport.populateDictionary(deliveryOrderModel));
    }

    @Test
    void populateDictionaryWithouConsolidation_BillingIntegrationDisabled() throws RunnerException {
        DeliveryOrderModel deliveryOrderModel = new DeliveryOrderModel();
        deliveryOrderModel.setUsersDto(UserContext.getUser());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).decimalPlaces(2).build());
        deliveryOrderModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        populateModel(deliveryOrderModel);
        ShipmentModel shipmentModel = deliveryOrderModel.shipmentDetails;
        shipmentModel.getPackingList().get(0).setHazardous(true);
        shipmentModel.getPackingList().get(0).setIsTemperatureControlled(true);
        deliveryOrderModel.setHbl(populateHbl());
        deliveryOrderModel.setTenantModel(new TenantModel());
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(deliveryOrderModel.shipmentDetails);
        mockBill(false);
        mockUnloc();
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(deliveryOrderReport.populateDictionary(deliveryOrderModel));
    }

    @Test
    void populateDictionaryWithoutConsolidation_BillingIntegrationEnabled() throws RunnerException {
        DeliveryOrderModel deliveryOrderModel = new DeliveryOrderModel();
        deliveryOrderModel.setUsersDto(UserContext.getUser());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).decimalPlaces(2).build());
        deliveryOrderModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        populateModel(deliveryOrderModel);
        ShipmentModel shipmentModel = deliveryOrderModel.shipmentDetails;
        shipmentModel.getPackingList().get(0).setHazardous(true);
        shipmentModel.getPackingList().get(0).setIsTemperatureControlled(true);
        deliveryOrderModel.setHbl(populateHbl());
        deliveryOrderModel.setTenantModel(new TenantModel());
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        masterDataMock();
        mockCarrier();
        mockRakc(deliveryOrderModel.shipmentDetails);
        mockBill(true);
        mockUnloc();
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(deliveryOrderReport.populateDictionary(deliveryOrderModel));
    }

    private void mockBill(boolean isBillingIntegrationEnabled) throws RunnerException {
        List<BillingResponse> billingResponseList = Arrays.asList(new BillingResponse());
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(billingResponseList).build();
        if(!isBillingIntegrationEnabled) {
            when(v1MasterData.fetchBillingList(any())).thenReturn(dependentServiceResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class)).thenReturn(billingResponseList);
        }

        List<BillChargesResponse> billChargesResponseList = new ArrayList<>();
        BillChargesResponse billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("PPD");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponseList.add(billChargesResponse);

        billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("CPP");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponseList.add(billChargesResponse);

        billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("CAL");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponseList.add(billChargesResponse);

        billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("SHW");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponseList.add(billChargesResponse);

        billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("ALL");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponseList.add(billChargesResponse);

        billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("CCL");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponseList.add(billChargesResponse);

        dependentServiceResponse = DependentServiceResponse.builder().data(billChargesResponseList).build();
        if(!isBillingIntegrationEnabled) {
            when(v1MasterData.fetchBillChargesList(any())).thenReturn(dependentServiceResponse);
        }

        Map<String, Object> dataMap = new HashMap<>();
        dataMap.put(CHARGE_TYPE_CODE, "AGENT CHARGE");
        if(!isBillingIntegrationEnabled) {
            doReturn(dataMap).when(jsonHelper).convertValue(any(BillChargesResponse.class), any(TypeReference.class));
        }

        NPMFetchLangChargeCodeResponse npmFetchLangChargeCodeResponse = new NPMFetchLangChargeCodeResponse();
        npmFetchLangChargeCodeResponse.setTranslation("AGENT CHARGE");

        if(!isBillingIntegrationEnabled) {
            when(npmServiceAdapter.fetchMultiLangChargeCode(any())).thenReturn(npmFetchLangChargeCodeResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class)).thenReturn(billChargesResponseList);
        }
    }

    private void masterDataMock() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new MasterData());
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);

        List<MasterData> masterDataList = new ArrayList<>();
        MasterData masterData = new MasterData();
        masterData.setItemType(MasterDataType.PAYMENT.getId());
        masterData.setItemValue("PPT");
        masterData.setItemDescription("PPT");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.SERVICE_MODE.getId());
        masterData.setItemValue("TXT");
        masterData.setItemDescription("TXT");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.TRANSPORT_MODE.getId());
        masterData.setItemValue(SEA);
        masterData.setItemDescription(SEA);
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId());
        masterData.setItemValue(EXP);
        masterData.setItemDescription(EXP);
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.PACKS_UNIT.getId());
        masterData.setItemValue("PKG");
        masterData.setItemDescription("PKG");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.VOLUME_UNIT.getId());
        masterData.setItemValue("M3");
        masterData.setItemDescription("M3");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.WEIGHT_UNIT.getId());
        masterData.setItemValue("KG");
        masterData.setItemDescription("KG");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.RELEASE_TYPE.getId());
        masterData.setItemValue("ORG");
        masterData.setItemDescription("ORG");
        masterDataList.add(masterData);

        masterData = new MasterData();
        masterData.setItemType(MasterDataType.COUNTRIES.getId());
        masterData.setItemValue("IND");
        masterData.setItemDescription("IND");
        masterDataList.add(masterData);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), MasterData.class)).thenReturn(masterDataList);
    }

    private void mockCarrier() {
        CarrierMasterData carrierMasterData = new CarrierMasterData();
        carrierMasterData.setIataCode("123");
        carrierMasterData.setItemDescription("123");
        carrierMasterData.setItemValue("Turkish Airlines");
        carrierMasterData.setDefaultOrgId(1);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(carrierMasterData)).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(carrierMasterData));
        when(masterDataUtils.fetchOrganizations(any(), any())).thenReturn(Arrays.asList(new EntityTransferOrganizations()));
    }

    private void mockUnloc() {
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(unlocationsResponse)).build();
        when(v1MasterData.fetchAllUnlocationData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), UnlocationsResponse.class)).thenReturn(Arrays.asList(unlocationsResponse));
    }

    private void mockUnlocation() {
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        unlocationsResponse.setAirPortName("Test");
        Map<String, UnlocationsResponse>  locationMap = new HashMap<>();
        locationMap.put(LOC_CODE, unlocationsResponse);
        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>(locationMap));
    }

    @Test
    void getDocumentModel() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(List.of(new ContainerModel()));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        shipmentModel.setConsolidationList(List.of(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(hblDao.findByShipmentId(any())).thenReturn(new ArrayList<>());
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = List.of(new UnlocationsResponse());
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Arrays.asList(new UnlocationsResponse()));
        V1MasterDataImpl mockV1MasterDataImpl = mock(V1MasterDataImpl.class);
        when(masterDataFactory.getMasterDataService()).thenReturn(mockV1MasterDataImpl);
        when(mockV1MasterDataImpl.retrieveTenant()).thenReturn(new DependentServiceResponse());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(deliveryOrderReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(List.of(new ContainerModel()));

        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> deliveryOrderReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        UserContext.getUser().getPermissions().put(PermissionConstants.AIR_SECURITY_PERMISSION, true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(List.of(new ContainerModel()));
        shipmentModel.setContainsHazardous(true);

        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> deliveryOrderReport.getDocumentModel(123L));
    }
}
