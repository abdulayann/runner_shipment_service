package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.AWbLabelModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.impl.ContainerService;
import com.dpw.runner.shipment.services.service.impl.PackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AWBLabelReportTest extends CommonMocks {

    @InjectMocks
    private AWBLabelReport awbLabelReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private IAwbDao awbDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private PackingService packingService;

    @MockBean
    private CommonUtils commonUtils;

    @Mock
    private ContainerService containerService;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().disableBlPartiesName(false).build());
    }


    private static ShipmentDetails shipmentDetails;
    private static ConsolidationDetails consolidationDetails;
    private static Awb hawb;
    private static Awb mawb;
    @BeforeEach
    void setup() {
        hawb = jsonTestUtility.getTestHawb();
        mawb = jsonTestUtility.getTestMawb();
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).build());
    }

    private void populateModel(AWbLabelModel aWbLabelModel) {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(123L);
        shipmentModel.setMasterBill("12");
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
        shipmentModel.setInnerPacks(123);

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
        carrierDetailModel.setDestinationPort("bb69aefb-0294-4be9-baec-835a431123df");
        carrierDetailModel.setDestination(carrierDetailModel.getDestinationPort());
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

        ShipmentContainers shipmentContainers = new ShipmentContainers();
        shipmentContainers.setContainerCount(1L);
        shipmentContainers.setContainerTypeCode("20GP");
        shipmentContainers.setNetWeight(BigDecimal.TEN);
        shipmentContainers.setNoofPackages(10L);
        shipmentModel.setShipmentContainersList(Arrays.asList(shipmentContainers));

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
        containers.setCarrierSealNumber("Test123");
        containerModelList.add(containers);

        containers = new ContainerModel();
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
        containers.setCarrierSealNumber("Test123");
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
        aWbLabelModel.setShipment(shipmentModel);

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
        packingModel2.setChargeable(BigDecimal.TEN);
        packingModel2.setChargeableUnit("KG");
        packingModels.add(packingModel2);
        shipmentModel.setPackingList(packingModels);

        List<TruckDriverDetailsModel> truckDriverDetailsModels = new ArrayList<>();
        TruckDriverDetailsModel truckDriverDetailsModel = new TruckDriverDetailsModel();
        truckDriverDetailsModel.setTransporterType(Ownership.Self);
        truckDriverDetailsModels.add(truckDriverDetailsModel);
        shipmentModel.setTruckDriverDetails(truckDriverDetailsModels);


        List<ReferenceNumbersModel> referenceNumbersModels = new ArrayList<>();
        ReferenceNumbersModel referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReferenceNumbersConstants.REF_NUM_TYPE_ETN);
        referenceNumbersModels.add(referenceNumbersModel);
        referenceNumbersModel = new ReferenceNumbersModel();
        referenceNumbersModel.setType(ReferenceNumbersConstants.REF_NUM_TYPE_CRR);
        referenceNumbersModels.add(referenceNumbersModel);
        shipmentModel.setReferenceNumbersList(referenceNumbersModels);

        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setPayment("PPM");
        consolidationModel.setReceivingAgent(partiesModel);
        consolidationModel.setSendingAgent(partiesModel);
        consolidationModel.setCarrierDetails(carrierDetailModel);
        consolidationModel.setPackingList(packingModels);
        ArrivalDepartureDetailsModel arrivalDepartureDetailsModel = new ArrivalDepartureDetailsModel();
        arrivalDepartureDetailsModel.setCTOId(partiesModel);
        arrivalDepartureDetailsModel.setLastForeignPort(UUID.randomUUID().toString());
        consolidationModel.setArrivalDetails(arrivalDepartureDetailsModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        consolidationModel.setShipmentType(EXP);
        consolidationModel.setDepartureDetails(arrivalDepartureDetailsModel);
        consolidationModel.setContainersList(shipmentModel.getContainersList());
        consolidationModel.setId(123L);
        consolidationModel.setMawb("2343");
        consolidationModel.setShipmentsList(Arrays.asList(shipmentModel));

        if(awbLabelReport.isMawb())
            aWbLabelModel.setAwb(mawb);
        else
            aWbLabelModel.setAwb(hawb);

        aWbLabelModel.setShipment(shipmentModel);
        aWbLabelModel.setConsolidation(consolidationModel);
        aWbLabelModel.getConsolidation().setConsoleGrossWeightAndUnit("100 KG");
        aWbLabelModel.getShipment().setWeightUnit("KG");
    }

    @Test
    void populateDictionary() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary5() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Collections.emptyList());
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }
    @Test
    void populateDictionary7() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setPackingList(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Collections.emptyList());
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary8() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setPackingList(Collections.emptyList());

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Collections.emptyList());
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary2() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
//        aWbLabelModel.getShipment().setCarrierDetails(null);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.getShipment().getCarrierDetails().setDestination(null);
        aWbLabelModel.getShipment().getCarrierDetails().setDestinationPort(null);
        aWbLabelModel.getShipment().getCarrierDetails().setOrigin(null);
        aWbLabelModel.setConsolidation(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(packingService.calculatePackSummary(any(), any(), any(),any())).thenReturn(new PackSummaryResponse());
//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary10() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(null);
        aWbLabelModel.setTenant(null);
//        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
//        aWbLabelModel.getShipment().setCarrierDetails(null);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.getShipment().setJobType("DRT");
        aWbLabelModel.getShipment().setTransportMode(AIR);
        aWbLabelModel.setConsolidation(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(packingService.calculatePackSummary(any(), any(), any(),any())).thenReturn(null);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary11() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(null);
        aWbLabelModel.setTenant(null);
//        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
//        aWbLabelModel.getShipment().setCarrierDetails(null);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.getShipment().setJobType("DRT");
        aWbLabelModel.getShipment().setTransportMode(AIR);
        aWbLabelModel.setConsolidation(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        doThrow(new RunnerException()).when(packingService).calculatePackSummary(any(), any(), any(),any());
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary6() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setCarrierDetails(null);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.setAwb(null);
        aWbLabelModel.getShipment().getPackingList().get(0).setPacks(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setDestination(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setDestinationPort(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setOrigin(null);
        aWbLabelModel.setConsolidation(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary3() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setCarrierDetails(null);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setDestination(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setDestinationPort(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setOrigin(null);
        aWbLabelModel.setConsolidation(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary4() throws RunnerException { // when its not mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setCarrierDetails(null);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.getShipment().setRoutingsList(new ArrayList<>());
//        aWbLabelModel.getShipment().getCarrierDetails().setDestination(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setDestinationPort(null);
//        aWbLabelModel.getShipment().getCarrierDetails().setOrigin(null);
        aWbLabelModel.setConsolidation(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Collections.emptyList());
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.getConsolidation().setAllocations(new AllocationsModel());

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb2() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.getConsolidation().setMawb("3435343");
        aWbLabelModel.setShipment(null);
        aWbLabelModel.getConsolidation().setCarrierDetails(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
//        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb2_throwsException() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.getConsolidation().setMawb("3435343");
        aWbLabelModel.setShipment(null);
        aWbLabelModel.getConsolidation().setCarrierDetails(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        doThrow(new RunnerException()).when(packingService).calculatePackSummary(any(), any(), any(),any());


//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
//        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb2_nullPackSummary() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.getConsolidation().setMawb("3435343");
        aWbLabelModel.setShipment(null);
        aWbLabelModel.getConsolidation().setCarrierDetails(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
//        doThrow(new RunnerException()).when(packingService).calculatePackSummary(any(), any(), any(),any());
        when(packingService.calculatePackSummary(any(), any(), any(),any())).thenReturn(null);

//        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
//        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb3() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.setShipment(null);
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOriginPort("bb69aefb-0294-4be9-baec-835a431123df2");
        carrierDetailModel.setDestinationPort("bb69aefb-0294-4be9-baec-835a431123df2");
        aWbLabelModel.getConsolidation().setCarrierDetails(carrierDetailModel);
        aWbLabelModel.getConsolidation().setMawb(null);
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setPod("bb69aefb-0294-4be9-baec-835a431123df");
        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel2.setCarrier("test2");
        routingsModel2.setPod("bb69aefb-0294-4be9-baec-835a431123df2");
        List<RoutingsModel> routingsModels = new ArrayList<>();
        routingsModels.add(routingsModel);
        routingsModels.add(routingsModel2);
        aWbLabelModel.getConsolidation().setRoutingsList(routingsModels);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Collections.emptyList());
//        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb4() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.setShipment(null);
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOriginPort("bb69aefb-0294-4be9-baec-835a431123df2");
        carrierDetailModel.setDestinationPort("bb69aefb-0294-4be9-baec-835a431123df2");
        aWbLabelModel.getConsolidation().setCarrierDetails(carrierDetailModel);
        aWbLabelModel.getConsolidation().setMawb(null);
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setPod("bb69aefb-0294-4be9-baec-835a431123df2");
        List<RoutingsModel> routingsModels = new ArrayList<>();
        routingsModels.add(routingsModel);
        aWbLabelModel.getConsolidation().setRoutingsList(routingsModels);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(Collections.emptyList());
//        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb5() throws RunnerException { // when its  mawb
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.setShipment(null);
        CarrierDetailModel carrierDetailModel = new CarrierDetailModel();
        carrierDetailModel.setOriginPort("bb69aefb-0294-4be9-baec-835a431123df2");
        carrierDetailModel.setDestinationPort("bb69aefb-0294-4be9-baec-835a431123df2");
        aWbLabelModel.getConsolidation().setCarrierDetails(carrierDetailModel);
        aWbLabelModel.getConsolidation().setMawb(null);
        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setPod("bb69aefb-0294-4be9-baec-835a431123df");
        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel2.setCarrier("test2");
        routingsModel2.setPod("bb69aefb-0294-4be9-baec-835a431123df1");
        RoutingsModel routingsModel3 = new RoutingsModel();
        routingsModel3.setLeg(3L);
        routingsModel3.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel3.setCarrier("test3");
        routingsModel3.setPod("abcd");
        List<RoutingsModel> routingsModels = new ArrayList<>();
        routingsModels.add(routingsModel);
        routingsModels.add(routingsModel2);
        routingsModels.add(routingsModel3);
        aWbLabelModel.getConsolidation().setRoutingsList(routingsModels);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df1");
        unlocationsResponse.setIataCode("test1");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponse.setIataCode("test");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("abcd");
        unlocationsResponse.setIataCode("test3");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(),
                "bb69aefb-0294-4be9-baec-835a431123df1", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test1").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenMawb6() throws RunnerException { // when its  mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.getShipment().getCarrierDetails().setDestination(null);
        aWbLabelModel.getShipment().getCarrierDetails().setOrigin(null);
        aWbLabelModel.setConsolidation(null);

        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setPod("bb69aefb-0294-4be9-baec-835a431123df2");
        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel2.setCarrier("test2");
        routingsModel2.setPod("bb69aefb-0294-4be9-baec-835a431123df1");
        RoutingsModel routingsModel3 = new RoutingsModel();
        routingsModel3.setLeg(3L);
        routingsModel3.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel3.setCarrier("test3");
        routingsModel3.setPod("abcd");
        List<RoutingsModel> routingsModels = new ArrayList<>();
        routingsModels.add(routingsModel);
        routingsModels.add(routingsModel2);
        routingsModels.add(routingsModel3);
        aWbLabelModel.getShipment().setRoutingsList(routingsModels);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df1");
        unlocationsResponse.setIataCode("test1");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df2");
        unlocationsResponse.setIataCode("test");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("abcd");
        unlocationsResponse.setIataCode("test3");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df2", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(),
                "bb69aefb-0294-4be9-baec-835a431123df1", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test1").build()));

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(packingService.calculatePackSummary(any(), any(), any(),any())).thenReturn(new PackSummaryResponse());
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }


    @Test
    void populateDictionary_whenMawb7() throws RunnerException { // when its  mawb
        awbLabelReport.setMawb(false);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        aWbLabelModel.getShipment().setInnerPacks(null);
        aWbLabelModel.getShipment().setNoOfPacks(null);
        aWbLabelModel.getShipment().getCarrierDetails().setDestination(null);
        aWbLabelModel.getShipment().getCarrierDetails().setOrigin(null);
        aWbLabelModel.setConsolidation(null);

        RoutingsModel routingsModel = new RoutingsModel();
        routingsModel.setLeg(1L);
        routingsModel.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel.setCarrier("test");
        routingsModel.setPod("bb69aefb-0294-4be9-baec-835a431123df");
        RoutingsModel routingsModel2 = new RoutingsModel();
        routingsModel2.setLeg(2L);
        routingsModel2.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel2.setCarrier("test2");
        routingsModel2.setPod("bb69aefb-0294-4be9-baec-835a431123df1");
        RoutingsModel routingsModel3 = new RoutingsModel();
        routingsModel3.setLeg(3L);
        routingsModel3.setMode(Constants.TRANSPORT_MODE_AIR);
        routingsModel3.setCarrier("test3");
        routingsModel3.setPod("abcd");
        List<RoutingsModel> routingsModels = new ArrayList<>();
        routingsModels.add(routingsModel);
        routingsModels.add(routingsModel2);
        routingsModels.add(routingsModel3);
        aWbLabelModel.getShipment().setRoutingsList(routingsModels);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df1");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("abcd");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(),
                "bb69aefb-0294-4be9-baec-835a431123df1", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test1").build()));

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(packingService.calculatePackSummary(any(), any(), any(),any())).thenReturn(new PackSummaryResponse());
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionaryWithMawbNull() throws RunnerException {
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        ShipmentModel shipmentModel = aWbLabelModel.shipment;
        shipmentModel.setMasterBill(null);

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionaryWithMasterBillLessThan11() throws RunnerException {
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        ShipmentModel shipmentModel = aWbLabelModel.shipment;
        ConsolidationModel consolidationModel = aWbLabelModel.getConsolidation();
        shipmentModel.setMasterBill("123456789999");

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void getDocumentModel() throws RunnerException {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setDirection(EXP);
        shipmentModel.setConsolidationList(Arrays.asList(new ConsolidationModel()));
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        shipmentModel.setCarrierDetails(new CarrierDetailModel());
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        assertNotNull(awbLabelReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_whenMAWB() throws RunnerException {
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        awbLabelReport.setMawb(true);

        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(aWbLabelModel.getConsolidation());

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacksWeight("500KG");
        when(packingService.calculatePackSummary(any(), any(), any(), any())).thenReturn(packSummaryResponse);
//        mockShipmentSettings();
        assertNotNull(awbLabelReport.getDocumentModel(123L));
    }

    @Test
    void getData() throws RunnerException {
        var spyReport = Mockito.spy(this.awbLabelReport);
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        doReturn(aWbLabelModel).when(spyReport).getDocumentModel(any());
        doReturn(new HashMap<>()).when(spyReport).populateDictionary(aWbLabelModel);
        var resp = spyReport.getData(123L);
        assertTrue(resp.isEmpty());
    }

    @Test
    void testInitialValues() {
        assertFalse(awbLabelReport.isMawb());
        assertEquals(null, awbLabelReport.getRemarks());
    }

    @Test
    void testSetAndIsMawb() {
        awbLabelReport.setMawb(true);
        assertTrue(awbLabelReport.isMawb());

        awbLabelReport.setMawb(false);
        assertFalse(awbLabelReport.isMawb());
    }

    @Test
    void testSetAndGetRemarks() {
        String remarks = "Sample Remarks";
        awbLabelReport.setRemarks(remarks);
        assertEquals(remarks, awbLabelReport.getRemarks());

        String newRemarks = "Updated Remarks";
        awbLabelReport.setRemarks(newRemarks);
        assertEquals(newRemarks, awbLabelReport.getRemarks());
    }

    @Test
    void testGetConsolGrossWeightAndUnit_WhenConsolidationDetailsIsPresent() throws RunnerException {
        // Arrange
        Long consoleId = 123L;
        ConsolidationModel consolidationModel = mock(ConsolidationModel.class);
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacksWeight("500KG");

        when(packingService.calculatePackSummary(any(), any(), any(), any())).thenReturn(packSummaryResponse);

        // Act
        String result = awbLabelReport.getConsolGrossWeightAndUnit(consolidationModel);

        // Assert
        assertEquals("500KG", result);
        verify(packingService).calculatePackSummary(any(), any(), any(), any());
    }

    @Test
    void testDocumentModelCombi() throws RunnerException {
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        awbLabelReport.setCombi(true);
        awbLabelReport.setMawb(true);

        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(aWbLabelModel.getConsolidation());
        when(modelMapper.map(any(), eq(ShipmentModel.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentModel.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacksWeight("500KG");
        when(packingService.calculatePackSummary(any(), any(), any(), any())).thenReturn(packSummaryResponse);
//        mockShipmentSettings();
        assertNotNull(awbLabelReport.getDocumentModel(123L));
    }


    @Test
    void testDocumentModelCombi2() throws RunnerException {
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        awbLabelReport.setCombi(true);


        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(aWbLabelModel.getConsolidation());
        when(modelMapper.map(any(), eq(ShipmentModel.class))).thenReturn(objectMapper.convertValue(shipmentDetails, ShipmentModel.class));

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacksWeight("500KG");
        when(packingService.calculatePackSummary(any(), any(), any(), any())).thenReturn(packSummaryResponse);
        assertNotNull(awbLabelReport.getDocumentModel(123L));
    }

    @Test
    void testDocumentModelCombi3() {
        awbLabelReport.setCombi(true);
        shipmentDetails.setConsolidationList(null);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        assertThrows(RunnerException.class, () ->awbLabelReport.getDocumentModel(123L));
    }

    @Test
    void testDocumentModelCombi4() throws RunnerException {
        awbLabelReport.setCombi(true);
        awbLabelReport.setCustomLabel(true);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);



        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacksWeight("500KG");
        assertNotNull(awbLabelReport.getDocumentModel(123L));
    }

    @Test
    void testDocumentModelCombi5() {
        awbLabelReport.setCombi(true);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        populateModel(aWbLabelModel);

        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setTotalPacksWeight("500KG");
        assertThrows(NullPointerException.class, () ->awbLabelReport.getDocumentModel(123L));
    }

    @Test
    void populateDictionary_whenCombi() throws RunnerException { // when its combi
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        awbLabelReport.setCombi(true);
        populateModel(aWbLabelModel);
        aWbLabelModel.getConsolidation().setAllocations(new AllocationsModel());

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

    @Test
    void populateDictionary_whenCombi_withpacks() throws RunnerException { // when its combi
        AWbLabelModel aWbLabelModel = new AWbLabelModel();
        aWbLabelModel.setTenantAddress(new ArrayList<>());
        aWbLabelModel.setTenant(new TenantModel());
        awbLabelReport.setMawb(true);
        awbLabelReport.setCombi(true);
        ShipmentModel shipmentModel = new ShipmentModel();
        PackingModel packingModel = new PackingModel();
        packingModel.setPacks("23");
        shipmentModel.setPackingList(List.of(packingModel));
        aWbLabelModel.setShipmentModels(List.of(shipmentModel));
        populateModel(aWbLabelModel);
        aWbLabelModel.getConsolidation().setAllocations(new AllocationsModel());

        List<UnlocationsResponse> unlocationsResponses = new ArrayList<>();
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("Kempegowda International Airport BLR");
        unlocationsResponse.setCountry("IND");
        unlocationsResponses.add(unlocationsResponse);
        unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setName("George Bush Intercontinental Airport IAH, TX");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setLocationsReferenceGUID("bb69aefb-0294-4be9-baec-835a431123df");
        unlocationsResponses.add(unlocationsResponse);

        when(masterDataUtils.getLocationData(any())).thenReturn(Map.of("test", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build(), "bb69aefb-0294-4be9-baec-835a431123df", UnlocationsResponse.builder().airPortName("name").portName("test").iataCode("test").build()));


        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = unlocationsResponses;
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), UnlocationsResponse.class)).thenReturn(unlocationsResponses);
        mockTenantSettings();
        assertNotNull(awbLabelReport.populateDictionary(aWbLabelModel));
    }

}
