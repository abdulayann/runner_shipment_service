package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ConsolidatedPackingListModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.reportingservice.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
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

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidatedPackingListReportTest extends CommonMocks {

    @InjectMocks
    private ConsolidatedPackingListReport consolidatedPackingListReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

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
    @BeforeEach
    void setup() {
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).build());
    }

    private void populateModel(ConsolidatedPackingListModel consolidatedPackingListModel) {
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
        orgData.put(ADDRESS1, "123");
        orgData.put(ADDRESS2, "123");
        orgData.put(CITY, "123");
        orgData.put(EMAIL, "123");
        orgData.put(CONTACT_PHONE, "123");
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
        consolidationModel.setShipmentsList(Arrays.asList(shipmentModel));
        consolidatedPackingListModel.setConsolidationDetails(consolidationModel);
    }

    @Test
    void populateDictionary() {
        ConsolidatedPackingListModel consolidatedPackingListModel = new ConsolidatedPackingListModel();
        populateModel(consolidatedPackingListModel);
        consolidatedPackingListModel.setTenant(new TenantModel());

        doReturn(consolidatedPackingListModel.getConsolidationDetails().getSendingAgent()).when(jsonHelper).convertValue(consolidatedPackingListModel.getConsolidationDetails().getSendingAgent(), PartiesModel.class);

        Map<String, Object> packMap = new HashMap<>();
        packMap.put(WEIGHT, BigDecimal.TEN);
        packMap.put(NET_WEIGHT, BigDecimal.TEN);
        packMap.put(VOLUME_WEIGHT, BigDecimal.TEN);
        packMap.put(WEIGHT_UNIT, "KG");
        packMap.put(VOLUME, BigDecimal.TEN);
        packMap.put(VOLUME_UNIT, "M3");
        packMap.put(PACKS, BigDecimal.ZERO);

        List<Map<String, Object>> packListMap = new ArrayList<>();
        packListMap.add(packMap);

        packMap = new HashMap<>();
        packMap.put(WEIGHT, BigDecimal.TEN);
        packMap.put(NET_WEIGHT, BigDecimal.TEN);
        packMap.put(VOLUME_WEIGHT, BigDecimal.TEN);
        packMap.put(WEIGHT_UNIT, "G");
        packMap.put(VOLUME, BigDecimal.TEN);
        packMap.put(VOLUME_UNIT, "CM3");
        packMap.put(PACKS, BigDecimal.ZERO);
        packListMap.add(packMap);

        doReturn(packListMap).when(jsonHelper).convertValue(any(HashSet.class), any(TypeReference.class));
        mockTenantSettings();
        assertNotNull(consolidatedPackingListReport.populateDictionary(consolidatedPackingListModel));
    }

    @Test
    void populateDictionaryWithConsolidationPack() {
        ConsolidatedPackingListModel consolidatedPackingListModel = new ConsolidatedPackingListModel();
        populateModel(consolidatedPackingListModel);
        consolidatedPackingListModel.setTenant(new TenantModel());
        ConsolidationModel consolidationModel = consolidatedPackingListModel.getConsolidationDetails();
        consolidationModel.setPackingList(consolidationModel.getShipmentsList().get(0).getPackingList());
        consolidationModel.setSendingAgent(null);
        consolidationModel.setReceivingAgent(null);
        consolidationModel.setReferenceNumbersList(null);

        doReturn(consolidatedPackingListModel.getConsolidationDetails().getSendingAgent()).when(jsonHelper).convertValue(consolidatedPackingListModel.getConsolidationDetails().getSendingAgent(), PartiesModel.class);

        Map<String, Object> packMap = new HashMap<>();
        packMap.put(WEIGHT, BigDecimal.TEN);
        packMap.put(NET_WEIGHT, BigDecimal.TEN);
        packMap.put(VOLUME_WEIGHT, BigDecimal.TEN);
        packMap.put(WEIGHT_UNIT, "KG");
        packMap.put(VOLUME, BigDecimal.TEN);
        packMap.put(VOLUME_UNIT, "M3");
        packMap.put(PACKS, BigDecimal.TEN);

        List<Map<String, Object>> packListMap = new ArrayList<>();
        packListMap.add(packMap);

        packMap = new HashMap<>();
        packMap.put(WEIGHT, BigDecimal.TEN);
        packMap.put(NET_WEIGHT, BigDecimal.TEN);
        packMap.put(VOLUME_WEIGHT, BigDecimal.TEN);
        packMap.put(WEIGHT_UNIT, "KG");
        packMap.put(VOLUME, BigDecimal.TEN);
        packMap.put(VOLUME_UNIT, "M3");
        packMap.put(PACKS, BigDecimal.ZERO);
        packListMap.add(packMap);

        doReturn(packListMap).when(jsonHelper).convertValue(any(HashSet.class), any(TypeReference.class));
        mockTenantSettings();
        assertNotNull(consolidatedPackingListReport.populateDictionary(consolidatedPackingListModel));
    }

    @Test
    void getDocumentModel() {
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(123L);
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(consolidationDetails);
        when(modelMapper.map(consolidationDetails, ConsolidationModel.class)).thenReturn(consolidationModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());

        assertNotNull(consolidatedPackingListReport.getDocumentModel(123L));
    }
}
