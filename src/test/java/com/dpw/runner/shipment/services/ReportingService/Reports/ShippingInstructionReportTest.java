package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.ShippingInstructionModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IHblRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ShippingInstructionReportTest extends CommonMocks {

    @InjectMocks
    private ShippingInstructionReport shippingInstructionReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private IHblRepository hblRepository;

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IShipmentDao shipmentDao;

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
        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.entities = Arrays.asList(new VesselsResponse());
        when(v1Service.fetchVesselData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.getEntities(), VesselsResponse.class)).thenReturn(Arrays.asList(new VesselsResponse()));
    }

    private void populateModel(ShippingInstructionModel shippingInstructionModel) {
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
        containers.setNoOfPackages(10L);
        containers.setContainerNumber("CONT000283");
        containers.setGrossVolume(BigDecimal.TEN);
        containers.setGrossVolumeUnit("M3");
        containers.setGrossWeight(BigDecimal.TEN);
        containers.setGrossWeightUnit("KG");
        containers.setPacksType("PKG");
        containers.setPacks("100");
        containerModelList.add(containers);
        shipmentModel.setContainersList(containerModelList);
        shippingInstructionModel.setContainersList(containerModelList);

        PickupDeliveryDetailsModel delivertDetails = new PickupDeliveryDetailsModel();
        delivertDetails.setActualPickupOrDelivery(LocalDateTime.now());
        delivertDetails.setDestinationDetail(partiesModel);
        delivertDetails.setAgentDetail(partiesModel);
        delivertDetails.setSourceDetail(partiesModel);
        delivertDetails.setTransporterDetail(partiesModel);
        shipmentModel.setPickupDetails(delivertDetails);
        shipmentModel.setDeliveryDetails(delivertDetails);
        shippingInstructionModel.setShipment(shipmentModel);

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
    }

    private void mockUnlocation() {
        UnlocationsResponse unlocationsResponse = new UnlocationsResponse();
        unlocationsResponse.setIataCode("Test");
        unlocationsResponse.setName("Test");
        unlocationsResponse.setCountry("IND");
        unlocationsResponse.setPortName("Test");
        when(masterDataUtils.getUNLocRow(any())).thenReturn(unlocationsResponse);
    }

    @Test
    void populateDictionary() {
        ShippingInstructionModel shippingInstructionModel = ShippingInstructionModel.builder().build();
        shippingInstructionModel.setTenant(new TenantModel());
        populateModel(shippingInstructionModel);
        mockVessel();
        mockUnlocation();
        Map<String, Object> packMap = new HashMap<>();
        packMap.put(WEIGHT, BigDecimal.TEN);
        packMap.put(NET_WEIGHT, BigDecimal.TEN);
        packMap.put(VOLUME_WEIGHT, BigDecimal.TEN);
        packMap.put(WEIGHT_UNIT, "KG");
        packMap.put(VOLUME, BigDecimal.TEN);
        packMap.put(VOLUME_UNIT, "M3");
        packMap.put(PACKS, BigDecimal.TEN);
        doReturn(packMap).when(jsonHelper).convertValue(eq(shippingInstructionModel.getShipment().getPackingList().get(0)), any(TypeReference.class));

        Map<String, Object> packMap2 = new HashMap<>();
        packMap2.put(WEIGHT, BigDecimal.TEN);
        packMap2.put(WEIGHT_UNIT, "G");
        packMap2.put(VOLUME, BigDecimal.TEN);
        packMap2.put(VOLUME_UNIT, "CM3");
        packMap2.put(PACKS, BigDecimal.TEN);
        doReturn(packMap2).when(jsonHelper).convertValue(eq(shippingInstructionModel.getShipment().getPackingList().get(1)), any(TypeReference.class));


        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        when(hblRepository.findByShipmentId(any())).thenReturn(Arrays.asList(hbl));
        mockShipmentSettings();
        assertNotNull(shippingInstructionReport.populateDictionary(shippingInstructionModel));
    }

    @Test
    void populateDictionaryWithoutRawData() {
        ShippingInstructionModel shippingInstructionModel = ShippingInstructionModel.builder().build();
        shippingInstructionModel.setTenant(new TenantModel());
        populateModel(shippingInstructionModel);
        ShipmentModel shipmentModel = shippingInstructionModel.getShipment();
        shipmentModel.setConsignee(null);
        shipmentModel.setConsigner(null);
        shipmentModel.getAdditionalDetails().setNotifyParty(null);

        mockVessel();
        mockUnlocation();
        Map<String, Object> packMap = new HashMap<>();
        packMap.put(WEIGHT, BigDecimal.TEN);
        packMap.put(NET_WEIGHT, BigDecimal.TEN);
        packMap.put(VOLUME_WEIGHT, BigDecimal.TEN);
        packMap.put(WEIGHT_UNIT, "KG");
        packMap.put(VOLUME, BigDecimal.TEN);
        packMap.put(VOLUME_UNIT, "M3");
        packMap.put(PACKS, BigDecimal.ZERO);
        doReturn(packMap).when(jsonHelper).convertValue(eq(shippingInstructionModel.getShipment().getPackingList().get(0)), any(TypeReference.class));

        Map<String, Object> packMap2 = new HashMap<>();
        packMap2.put(WEIGHT, BigDecimal.TEN);
        packMap2.put(WEIGHT_UNIT, "KG");
        packMap2.put(VOLUME, BigDecimal.TEN);
        packMap2.put(VOLUME_UNIT, "M3");
        packMap2.put(PACKS, BigDecimal.ZERO);
        doReturn(packMap2).when(jsonHelper).convertValue(eq(shippingInstructionModel.getShipment().getPackingList().get(1)), any(TypeReference.class));


        Hbl hbl = new Hbl();
        hbl.setHblData(new HblDataDto());
        when(hblRepository.findByShipmentId(any())).thenReturn(Arrays.asList(hbl));
        mockShipmentSettings();
        assertNotNull(shippingInstructionReport.populateDictionary(shippingInstructionModel));
    }

    @Test
    void getDocumentModel() {
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setTransportMode(SEA);
        shipmentModel.setDirection(EXP);
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        assertNotNull(shippingInstructionReport.getDocumentModel(123L));
    }

}
