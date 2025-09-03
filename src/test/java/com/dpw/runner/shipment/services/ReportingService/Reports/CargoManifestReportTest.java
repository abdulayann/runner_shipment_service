package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
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
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CargoManifestReportTest extends CommonMocks {
    @InjectMocks
    private CargoManifestReport cargoManifestReport;

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
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private HblReport hblReport;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private IAwbRepository awbRepository;

    @Mock
    private IAwbDao awbDao;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private CargoManifestModel cargoManifestModel;

    @Mock
    private AwbCargoInfo cargoInfoRows;

    @Mock
    private V1TenantSettingsResponse v1TenantSettingsResponse;

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
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).GSTTaxAutoCalculation(true).build());
    }

    @Test
    void populateDictionary() {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();

        cargoManifestModel.tenantDetails =  new TenantModel();
        cargoManifestModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        cargoManifestModel.usersDto = UserContext.getUser();
        cargoManifestModel.awb = new Awb();
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        awbCargoInfo.setSci("test");
        cargoManifestModel.awb.setAwbCargoInfo(awbCargoInfo);
        cargoManifestModel.awb.setOriginalPrintedAt(LocalDateTime.now());

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
        shipmentModel.setSecurityStatus("Test");
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setPacksUnit("PKG");

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setOrgCode("Test");
        partiesModel.setAddressCode("Test");
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
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));

        List<ContainerModel> containerModelList = new ArrayList<>();
        ContainerModel shipmentContainers = new ContainerModel();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setContainerNumber("CONT000283");
        shipmentContainers.setGrossVolume(BigDecimal.TEN);
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeight(BigDecimal.TEN);
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setPacksType("PKG");
        shipmentContainers.setPacks("100");
        containerModelList.add(shipmentContainers);
        shipmentModel.setContainersList(containerModelList);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        cargoManifestModel.shipmentDetails = shipmentModel;

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(new HashMap<>());

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

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));
        mockTenantSettings();
        assertNotNull(cargoManifestReport.populateDictionary(cargoManifestModel));
    }

    @Test
    void populateDictionary_haz_temp() {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();

        cargoManifestModel.tenantDetails =  new TenantModel();
        cargoManifestModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        cargoManifestModel.usersDto = UserContext.getUser();
        cargoManifestModel.awb = new Awb();
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        awbCargoInfo.setSci("test");
        cargoManifestModel.awb.setAwbCargoInfo(awbCargoInfo);
        cargoManifestModel.awb.setOriginalPrintedAt(LocalDateTime.now());

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
        shipmentModel.setSecurityStatus("Test");
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setPacksUnit("PKG");

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setOrgCode("Test");
        partiesModel.setAddressCode("Test");
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
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));

        List<ContainerModel> containerModelList = new ArrayList<>();
        ContainerModel shipmentContainers = new ContainerModel();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setContainerNumber("CONT000283");
        shipmentContainers.setGrossVolume(BigDecimal.TEN);
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeight(BigDecimal.TEN);
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setPacksType("PKG");
        shipmentContainers.setPacks("100");
        containerModelList.add(shipmentContainers);
        shipmentModel.setContainersList(containerModelList);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        cargoManifestModel.shipmentDetails = shipmentModel;

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        packingModel.setHazardous(true);
        packingModel.setIsTemperatureControlled(true);
        packingModel.setUnNumber("unNum");
        packingModel.setProperShippingName("psn");
        packingModel.setDGClass("1.1");
        packingModel.setMinimumFlashPoint(BigDecimal.ONE);
        packingModel.setMinimumFlashPointUnit("K");
        packingModel.setPackingGroup("packingGrp");
        packingModel.setMarinePollutant(true);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(new HashMap<>());

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

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));
        when(masterDataUtils.fetchDgSubstanceRow(any())).thenReturn(new EntityTransferDGSubstance());
        mockTenantSettings();
        when(cacheManager.getCache(any())).thenReturn(cache);
        when(cache.get(any())).thenReturn(null);
        assertNotNull(cargoManifestReport.populateDictionary(cargoManifestModel));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setDirection(EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        mockShipmentSettings();
        assertNotNull(cargoManifestReport.getDocumentModel(123L));
    }

    @Test
    void testOriginalPrintedAtDateTime() {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();
        cargoManifestModel.awb = new Awb();

        LocalDateTime now = LocalDateTime.now();
        cargoManifestModel.awb.setOriginalPrintedAt(now);
        LocalDateTime dateTime = cargoManifestModel.awb.getOriginalPrintedAt() != null
                ? cargoManifestModel.awb.getOriginalPrintedAt()
                : null;
        assertNotNull(dateTime, "dateTime should not be null when getOriginalPrintedAt is set");
        assertEquals(now, dateTime, "dateTime should match the set value");

        cargoManifestModel.awb.setOriginalPrintedAt(null);
        dateTime = cargoManifestModel.awb.getOriginalPrintedAt() != null
                ? cargoManifestModel.awb.getOriginalPrintedAt()
                : null;
        assertNull(dateTime, "dateTime should be null when getOriginalPrintedAt returns null");
    }

    @Test
    void populateDictionaryNewTags() {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();

        cargoManifestModel.tenantDetails =  new TenantModel();
        cargoManifestModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        cargoManifestModel.usersDto = UserContext.getUser();
        cargoManifestModel.awb = new Awb();
        AwbCargoInfo awbCargoInfo = new AwbCargoInfo();
        awbCargoInfo.setSci("test");
        cargoManifestModel.awb.setAwbCargoInfo(awbCargoInfo);
        cargoManifestModel.awb.setOriginalPrintedAt(LocalDateTime.now());

        List<RoutingsModel> routs = new ArrayList<>();
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(2L);
        routingsModel.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routingsModel.setMode(Constants.TRANSPORT_MODE_SEA);
        routingsModel.setVesselName("test");
        routingsModel.setVoyage("test");
        routingsModel.setCarrier("test");
        routingsModel.setPod("test");
        routingsModel.setPol("test");
        LocalDateTime localDate = LocalDateTime.now();
        routingsModel.setEta(localDate);
        routingsModel.setEtd(localDate);
        routs.add(routingsModel);

        List<ReferenceNumbersModel> modelList = new ArrayList<>();
        ReferenceNumbersModel refModel = new ReferenceNumbersModel();
        refModel.setType(CEN);
        refModel.setReferenceNumber("12345");
        modelList.add(refModel);

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
        shipmentModel.setSecurityStatus("Test");
        shipmentModel.setPaymentTerms("PPT");
        shipmentModel.setPacksUnit("PKG");
        shipmentModel.setWeightUnit("KG");
        shipmentModel.setVolumetricWeightUnit("M3");
        shipmentModel.setChargeableUnit("KG");
        shipmentModel.setBookingNumber("12345");
        shipmentModel.setServiceType("A2A");
        shipmentModel.setRoutingsList(routs);
        shipmentModel.setReferenceNumbersList(modelList);

        PartiesModel partiesModel = new PartiesModel();
        partiesModel.setOrgCode("Test");
        partiesModel.setAddressCode("Test");
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
        shipmentModel.setCarrierDetails(carrierDetailModel);
        shipmentModel.setAdditionalDetails(additionalDetailModel);
        shipmentModel.setShipmentAddresses(Arrays.asList(partiesModel));

        List<ContainerModel> containerModelList = new ArrayList<>();
        ContainerModel shipmentContainers = new ContainerModel();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setContainerNumber("CONT000283");
        shipmentContainers.setGrossVolume(BigDecimal.TEN);
        shipmentContainers.setGrossVolumeUnit("M3");
        shipmentContainers.setGrossWeight(BigDecimal.TEN);
        shipmentContainers.setGrossWeightUnit("KG");
        shipmentContainers.setPacksType("PKG");
        shipmentContainers.setPacks("100");
        containerModelList.add(shipmentContainers);
        shipmentModel.setContainersList(containerModelList);

        List<ConsolidationModel> consolidationModels = new ArrayList<>();
        ConsolidationModel model = new ConsolidationModel();
        model.setConsolidationNumber("AIR-CAN-00001");
        model.setAgentReference("test");
        ArrivalDepartureDetailsModel arrivalDepartureDetailsModel = new ArrivalDepartureDetailsModel();
        arrivalDepartureDetailsModel.setCTOId(null);
        String randomUUID  = UUID.randomUUID().toString();
        arrivalDepartureDetailsModel.setLastForeignPort(randomUUID);
        model.setArrivalDetails(arrivalDepartureDetailsModel);
        consolidationModels.add(model);
        shipmentModel.setConsolidationList(consolidationModels);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        cargoManifestModel.shipmentDetails = shipmentModel;

        BookingCarriageModel bookingCarriageModel = new BookingCarriageModel();
        bookingCarriageModel.setCarriageType(PRE_CARRIAGE);
        shipmentModel.setBookingCarriagesList(Arrays.asList(bookingCarriageModel));

        PackingModel packingModel = new PackingModel();
        packingModel.setLength(BigDecimal.TEN);
        packingModel.setWidth(BigDecimal.TEN);
        packingModel.setHeight(BigDecimal.TEN);
        shipmentModel.setPackingList(Arrays.asList(packingModel));

        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        unlocationsResponse.setLocCode("test");

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

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        doReturn(containerMap).when(jsonHelper).convertValue(any(ShipmentContainers.class), any(TypeReference.class));
        mockTenantSettings();

        String PWeight = BigDecimal.TEN + " KG";
        String PVolume = BigDecimal.TEN + " M3";
        String chargable = BigDecimal.TEN + " KG";
        String packsUnit = "PKG";
        String carrBookingRef = "12345";
        String serviceType = "A2A";
        String customEntryNumber = "12345";
        String test  = "test";
        String consolNumber = "AIR-CAN-00001";
        EntityTransferUnLocations entityTransferUnLocations = new EntityTransferUnLocations();
        entityTransferUnLocations.setLocCode("TestForeignPort");
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = new HashMap<>();
        entityTransferUnLocationsMap.put("TestForeignPort", entityTransferUnLocations);
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setLocCode("TestForeignPort");
        doReturn(unlocationsResponse1).when(jsonHelper).convertValue(any(EntityTransferUnLocations.class), eq(UnlocationsResponse.class));
        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(entityTransferUnLocationsMap);

        Map<String, Object> dictionary = cargoManifestReport.populateDictionary(cargoManifestModel);
        assertNotNull(dictionary);

        List<Map<String, Object>> routing = (List<Map<String, Object>>) dictionary.get(ROUTINGS);
        Map<String, Object> routsMap = routing.get(0);
        List<String> ctoAddress = ((List<String>) dictionary.get(CTO_ADDRESS));

        assertEquals(test, dictionary.get(AGENT_REFERENCE));
        assertEquals(consolNumber, dictionary.get(CONSOLIDATION_NUMBER));
        assertEquals(Constants.TRANSPORT_MODE_SEA, routsMap.get(MODE));
        assertEquals(0, ctoAddress.size());
        assertEquals(test, routsMap.get(VOYAGE));
        assertEquals(test, routsMap.get(CARRIER));
        assertEquals(test, routsMap.get(POLCODE));
        assertEquals(test, routsMap.get(PODCODE));
        assertEquals(customEntryNumber, dictionary.get(CUSTOMS_ENTRY_NUMBER));
        assertEquals(10, dictionary.get(TOTAL_PACKAGES));
        assertEquals(serviceType, dictionary.get(SERVICE_LEVEL));
        assertEquals(packsUnit, dictionary.get(PACKS_UNIT));
    }

}
