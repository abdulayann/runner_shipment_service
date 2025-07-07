package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.PickUpOrderReportModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
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
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXP;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PickupOrderReportTest extends CommonMocks {

    @InjectMocks
    private PickupOrderReport pickupOrderReport;
    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private HblReport hblReport;

    @Mock
    IShipmentDao shipmentDao;

    @Mock
    private IContainerDao containerDao;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setEnableTimeZone(false);
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

    private void populateModel(HblModel hblModel) {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .agentDetail(new PartiesModel())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
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
        hblModel.setCommonContainers(shipmentModel.getShipmentContainersList());

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

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        hblModel.setShipment(shipmentModel);

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
        consolidationModel.setArrivalDetails(arrivalDepartureDetailsModel);
        partiesModel = new PartiesModel();
        partiesModel.setType("Notify Party 1");
        orgData = new HashMap<>();
        orgData.put(FULL_NAME, "123");
        partiesModel.setOrgData(orgData);
        partiesModel.setAddressData(orgData);
        consolidationModel.setConsolidationAddresses(Arrays.asList(partiesModel));
        consolidationModel.setReferenceNumbersList(shipmentModel.getReferenceNumbersList());
        hblModel.setConsolidation(consolidationModel);
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
    void populateDictionary() {
        PickUpOrderReportModel pickUpOrderReportModel = new PickUpOrderReportModel();
        pickUpOrderReportModel.hblModel = new HblModel();
        pickUpOrderReportModel.hblModel.user = UserContext.getUser();
        pickUpOrderReportModel.hblModel.shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        pickUpOrderReportModel.hblModel.tenant = new TenantModel();
        populateModel(pickUpOrderReportModel.hblModel);
        pickUpOrderReportModel.pickUpTransportAddress = pickUpOrderReportModel.hblModel.shipment.getPickupDetails().getTransporterDetail();

        Map<String, Object> containerMap = new HashMap<>();
        containerMap.put(GROSS_VOLUME, BigDecimal.TEN);
        containerMap.put(GROSS_WEIGHT, BigDecimal.TEN);
        containerMap.put(SHIPMENT_PACKS, BigDecimal.TEN);
        containerMap.put(TARE_WEIGHT, BigDecimal.TEN);
        containerMap.put(VGM_WEIGHT, BigDecimal.TEN);
        containerMap.put(NET_WEIGHT, BigDecimal.TEN);
        mockRakc(pickUpOrderReportModel.hblModel.getShipment());
        Map<String, Object> dictionary = new HashMap<>();
        Map<String, Object> chargeMap = new HashMap<>();
        chargeMap.put(CHARGE_TYPE_CODE, "AgentCharge");
        dictionary.put(CHARGES_SMALL, Arrays.asList(chargeMap));
        when(hblReport.populateDictionary(any())).thenReturn(dictionary);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        mockTenantSettings();
        assertNotNull(pickupOrderReport.populateDictionary(pickUpOrderReportModel));
    }

    @Test
    void getDocumentModel() {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                        .id(12L)
                        .agentDetail(new PartiesModel())
                        .actualPickup(LocalDateTime.now())
                        .actualDelivery(LocalDateTime.now())
                .build()));
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        shipmentModel.setPickupDetails(new PickupDeliveryDetailsModel());
        ConsolidationModel consolidationModel = new ConsolidationModel();
        consolidationModel.setId(123L);
        consolidationModel.setPlaceOfIssue("Test");
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        HblModel hblModel = new HblModel();
        hblModel.setShipment(shipmentModel);
        when(hblReport.getDocumentModel(any())).thenReturn(hblModel);
        mockShipmentSettings();
        assertNotNull(pickupOrderReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .agentDetail(new PartiesModel())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        shipmentModel.setPickupDetails(new PickupDeliveryDetailsModel());
        HblModel hblModel = new HblModel();
        hblModel.setShipment(shipmentModel);

        when(hblReport.getDocumentModel(any())).thenReturn(hblModel);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> pickupOrderReport.getDocumentModel(123L));
    }

    @Test
    void getDocumentModel_CountryAirCargoSecurity2() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        UserContext.getUser().getPermissions().put(PermissionConstants.AIR_DG, true);
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(AIR);
        shipmentModel.setTransportInstructionId(12L);
        shipmentModel.setPickupDeliveryDetailsInstructions(List.of(PickupDeliveryDetailsModel.builder()
                .id(12L)
                .agentDetail(new PartiesModel())
                .actualPickup(LocalDateTime.now())
                .actualDelivery(LocalDateTime.now())
                .build()));
        shipmentModel.setDirection(EXP);
        shipmentModel.setContainersList(Arrays.asList(new ContainerModel()));
        shipmentModel.setPickupDetails(new PickupDeliveryDetailsModel());
        shipmentModel.setContainsHazardous(true);
        HblModel hblModel = new HblModel();
        hblModel.setShipment(shipmentModel);

        when(hblReport.getDocumentModel(any())).thenReturn(hblModel);
        mockShipmentSettings();

        assertThrows(ValidationException.class, () -> pickupOrderReport.getDocumentModel(123L));
    }
}
