package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ArrivalNoticeModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
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
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ArrivalNoticeReportTest extends CommonMocks {

    @InjectMocks
    private ArrivalNoticeReport arrivalNoticeReport;

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
    private CacheManager cacheManager;

    @Mock
    private Cache cache;

    @Mock
    private Cache.ValueWrapper valueWrapper;

    @Mock
    private CustomKeyGenerator keyGenerator;

    private static final String DG_CLASS_VALUE = "DG";

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
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).build());
    }

    private void mockVessel() {
        when(masterDataUtils.getVesselDataFromCache(any())).thenReturn(new HashMap<>());
    }

    private void populateModel(ArrivalNoticeModel arrivalNoticeModel) {
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
        partiesModel.setOrgCode("Test");
        partiesModel.setAddressCode("Test");
        partiesModel.setType(CUSTOM_HOUSE_AGENT);
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        orgData.put(CONTACT_PERSON, "123");
        orgData.put(COMPANY_NAME, "123");
        orgData.put(PartiesConstants.RAW_DATA, "Text");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);

        shipmentModel.setConsignee(partiesModel);
        shipmentModel.setConsigner(partiesModel);
        shipmentModel.setClient(partiesModel);

        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOrigin("test");
        carrierDetailModel.setOriginPort("test");
        carrierDetailModel.setEta(LocalDateTime.now());
        carrierDetailModel.setEtd(LocalDateTime.now());
        carrierDetailModel.setAtd(LocalDateTime.now());
        carrierDetailModel.setVessel(UUID.randomUUID().toString());
        carrierDetailModel.setAta(LocalDateTime.now());
        carrierDetailModel.setDestinationPort(UUID.randomUUID().toString());
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
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));
        shipmentModel.setServiceType("Test");

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));
        arrivalNoticeModel.setContainers(shipmentModel.getShipmentContainersList());

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
        containers.setDgClass(DG_CLASS_VALUE);
        containers.setTareWeight(BigDecimal.TEN);
        containers.setCommodityCode("BAG");
        containers.setCommodityGroup("BAG");
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
        arrivalNoticeModel.setShipmentDetails(shipmentModel);

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
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        arrivalNoticeModel.setConsolidationDetails(consolidationModel);
    }

    private Hbl populateHbl(){
        Hbl hbl = new Hbl();
        HblDataDto hblDataDto = new HblDataDto();
        hblDataDto.setCargoGrossVolumeUnit("M3");
        hblDataDto.setCargoGrossWeightUnit("KG");
        hblDataDto.setPackageCount(10);
        hbl.setHblData(hblDataDto);
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
    }

    @Test
    void populateDictionary_BillingIntegrationDisabled() {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.setUsersDto(UserContext.getUser());
        populateModel(arrivalNoticeModel);
        arrivalNoticeModel.setHbl(populateHbl());
        arrivalNoticeModel.setArrivalNoticeBillCharges(Arrays.asList(new ArrivalNoticeModel.ArrivalNoticeBillCharges()));
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(v1MasterData.retrieveTenant()).thenReturn(DependentServiceResponse.builder().data(new TenantModel()).build());

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);

        Map<String, EntityTransferMasterLists> dataMap = new HashMap<>();
        EntityTransferMasterLists entityTransferMasterLists = new EntityTransferMasterLists();
        entityTransferMasterLists.setValuenDesc("Test");
        dataMap.put(MasterDataType.COUNTRIES.getDescription(), new EntityTransferMasterLists());
        dataMap.put(DG_CLASS_VALUE + '#' + MasterDataType.masterData(MasterDataType.DG_CLASS.getId()).name(), new EntityTransferMasterLists());
        when(masterDataUtils.fetchInBulkMasterList(any())).thenReturn(dataMap);

        masterDataMock();
        mockCarrier();
        mockRakc(arrivalNoticeModel.shipmentDetails);
        mockBill(false);
        mockCommodity();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(arrivalNoticeReport.populateDictionary(arrivalNoticeModel));
    }

    @Test
    void populateDictionary_BillingIntegrationDisabled_haz_temp() {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.setUsersDto(UserContext.getUser());
        populateModel(arrivalNoticeModel);
        arrivalNoticeModel.getShipmentDetails().getPackingList().get(0).setHazardous(true);
        arrivalNoticeModel.getShipmentDetails().getPackingList().get(0).setIsTemperatureControlled(true);
        arrivalNoticeModel.setHbl(populateHbl());
        arrivalNoticeModel.setArrivalNoticeBillCharges(Arrays.asList(new ArrivalNoticeModel.ArrivalNoticeBillCharges()));
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(v1MasterData.retrieveTenant()).thenReturn(DependentServiceResponse.builder().data(new TenantModel()).build());

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);

        Map<String, EntityTransferMasterLists> dataMap = new HashMap<>();
        EntityTransferMasterLists entityTransferMasterLists = new EntityTransferMasterLists();
        entityTransferMasterLists.setValuenDesc("Test");
        dataMap.put(MasterDataType.COUNTRIES.getDescription(), new EntityTransferMasterLists());
        dataMap.put(DG_CLASS_VALUE + '#' + MasterDataType.masterData(MasterDataType.DG_CLASS.getId()).name(), new EntityTransferMasterLists());
        when(masterDataUtils.fetchInBulkMasterList(any())).thenReturn(dataMap);
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());

        masterDataMock();
        mockCarrier();
        mockRakc(arrivalNoticeModel.shipmentDetails);
        mockBill(false);
        mockCommodity();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(arrivalNoticeReport.populateDictionary(arrivalNoticeModel));
    }

    @Test
    void populateDictionary_BillingIntegrationEnabled() {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.setUsersDto(UserContext.getUser());
        populateModel(arrivalNoticeModel);
        arrivalNoticeModel.setHbl(populateHbl());
        arrivalNoticeModel.setArrivalNoticeBillCharges(Arrays.asList(new ArrivalNoticeModel.ArrivalNoticeBillCharges()));
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(v1MasterData.retrieveTenant()).thenReturn(DependentServiceResponse.builder().data(new TenantModel()).build());

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);

        Map<String, EntityTransferMasterLists> dataMap = new HashMap<>();
        EntityTransferMasterLists entityTransferMasterLists = new EntityTransferMasterLists();
        entityTransferMasterLists.setValuenDesc("Test");
        dataMap.put(MasterDataType.COUNTRIES.getDescription(), new EntityTransferMasterLists());
        dataMap.put(DG_CLASS_VALUE + '#' + MasterDataType.masterData(MasterDataType.DG_CLASS.getId()).name(), new EntityTransferMasterLists());
        when(masterDataUtils.fetchInBulkMasterList(any())).thenReturn(dataMap);

        masterDataMock();
        mockCarrier();
        mockRakc(arrivalNoticeModel.shipmentDetails);
        mockBill(true);
        mockCommodity();
        mockShipmentSettings();
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        when(keyGenerator.customCacheKeyForMasterData(any(),any())).thenReturn(new StringBuilder());
        assertNotNull(arrivalNoticeReport.populateDictionary(arrivalNoticeModel));
    }

    @Test
    void populateDictionaryWithoutConsolidation_BillingIntegrationDisabled() {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.setUsersDto(UserContext.getUser());
        populateModel(arrivalNoticeModel);
        arrivalNoticeModel.setHbl(populateHbl());
        arrivalNoticeModel.setArrivalNoticeBillCharges(Arrays.asList(new ArrivalNoticeModel.ArrivalNoticeBillCharges()));
        arrivalNoticeModel.consolidationDetails = null;
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(v1MasterData.retrieveTenant()).thenReturn(DependentServiceResponse.builder().data(new TenantModel()).build());
        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(cacheManager.getCache(any())).thenReturn(cache);

        Map<String, EntityTransferMasterLists> dataMap = new HashMap<>();
        EntityTransferMasterLists entityTransferMasterLists = new EntityTransferMasterLists();
        entityTransferMasterLists.setValuenDesc("Test");
        dataMap.put(MasterDataType.COUNTRIES.getDescription(), new EntityTransferMasterLists());
        dataMap.put(DG_CLASS_VALUE + '#' + MasterDataType.masterData(MasterDataType.DG_CLASS.getId()).name(), new EntityTransferMasterLists());
        when(cache.get(any())).thenReturn(valueWrapper);
        when(valueWrapper.get()).thenReturn(entityTransferMasterLists);

        masterDataMock();
        mockCarrier();
        mockRakc(arrivalNoticeModel.shipmentDetails);
        mockBill(false);
        mockCommodity();
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(arrivalNoticeReport.populateDictionary(arrivalNoticeModel));
    }

    @Test
    void populateDictionaryWithoutConsolidation_BillingIntegrationEnabled() {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.setUsersDto(UserContext.getUser());
        populateModel(arrivalNoticeModel);
        arrivalNoticeModel.setHbl(populateHbl());
        arrivalNoticeModel.setArrivalNoticeBillCharges(Arrays.asList(new ArrivalNoticeModel.ArrivalNoticeBillCharges()));
        arrivalNoticeModel.consolidationDetails = null;
        mockVessel();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        when(v1MasterData.retrieveTenant()).thenReturn(DependentServiceResponse.builder().data(new TenantModel()).build());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(cacheManager.getCache(any())).thenReturn(cache);

        Map<String, EntityTransferMasterLists> dataMap = new HashMap<>();
        EntityTransferMasterLists entityTransferMasterLists = new EntityTransferMasterLists();
        entityTransferMasterLists.setValuenDesc("Test");
        dataMap.put(MasterDataType.COUNTRIES.getDescription(), new EntityTransferMasterLists());
        dataMap.put(DG_CLASS_VALUE + '#' + MasterDataType.masterData(MasterDataType.DG_CLASS.getId()).name(), new EntityTransferMasterLists());
        when(cache.get(any())).thenReturn(valueWrapper);
        when(valueWrapper.get()).thenReturn(entityTransferMasterLists);

        masterDataMock();
        mockCarrier();
        mockRakc(arrivalNoticeModel.shipmentDetails);
        mockBill(true);
        mockCommodity();
        mockShipmentSettings();
        mockTenantSettings();
        assertNotNull(arrivalNoticeReport.populateDictionary(arrivalNoticeModel));
    }

    private void mockBill(boolean isBillingIntegrationEnabled) {
        List<BillingResponse> billingResponseList = Arrays.asList(new BillingResponse());
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(billingResponseList).build();
        if(!isBillingIntegrationEnabled) {
            when(v1MasterData.fetchBillingList(any())).thenReturn(dependentServiceResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class)).thenReturn(billingResponseList);
        }
        BillChargesResponse billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.Chargeable.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");

        List<BillChargesResponse> billChargesResponseList = Arrays.asList(billChargesResponse);
        dependentServiceResponse = DependentServiceResponse.builder().data(billChargesResponseList).build();
        if (!isBillingIntegrationEnabled) {
            when(v1MasterData.fetchBillChargesList(any())).thenReturn(dependentServiceResponse);
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
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(carrierMasterData)).build();
        when(v1MasterData.fetchCarrierMasterData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CarrierMasterData.class)).thenReturn(Arrays.asList(carrierMasterData));
    }

    private void mockCommodity() {
        CommodityResponse commodityResponse = new CommodityResponse();
        commodityResponse.setCode("123");
        commodityResponse.setCommodityDescriptionWithHSCode("123");
        commodityResponse.setDescription("Turkish Airlines");
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(Arrays.asList(commodityResponse)).build();
        when(v1MasterData.fetchCommodityData(any())).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), CommodityResponse.class)).thenReturn(Arrays.asList(commodityResponse));
    }

    @Test
    void getDocumentModel() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(hblDao.findByShipmentId(any())).thenReturn(new ArrayList<>());
        assertNotNull(arrivalNoticeReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel1() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        mockShipmentSettings();
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED);
        ContainerModel containerModel = new ContainerModel();
        containerModel.setHazardous(true);
        shipmentModel.setContainersList(Arrays.asList(containerModel));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        when(hblDao.findByShipmentId(any())).thenReturn(new ArrayList<>());
        mockTenantSettings();
        assertNotNull(arrivalNoticeReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel2() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        mockShipmentSettings();
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED);
        ContainerModel containerModel = new ContainerModel();
        shipmentModel.setContainersList(Arrays.asList(containerModel));
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        assertThrows(com.dpw.runner.shipment.services.exception.exceptions.ValidationException.class, () -> arrivalNoticeReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel3() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        mockShipmentSettings();
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainsHazardous(true);
        shipmentModel.setOceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        assertThrows(com.dpw.runner.shipment.services.exception.exceptions.ValidationException.class, () -> arrivalNoticeReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel4() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        mockShipmentSettings();
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainsHazardous(true);
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);
        assertThrows(com.dpw.runner.shipment.services.exception.exceptions.ValidationException.class, () -> arrivalNoticeReport.getDocumentModel(123L));
    }
}
