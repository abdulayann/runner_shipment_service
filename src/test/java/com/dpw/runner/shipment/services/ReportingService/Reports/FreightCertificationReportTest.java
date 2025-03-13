package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMPANY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PERSON;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOM_HOUSE_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.INVNO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_EXPIRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KNOWN_CONSIGNOR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ONE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PRE_CARRIAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REGULATED_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SEA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TWO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.numberToWords;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.FreightCertificationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.AdditionalDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ArrivalDepartureDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.CarrierDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.BillChargeCostDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.BillChargeRevenueDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.CurrencyExchangeRateDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.TaxDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.response.ArObjectResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.ChargeTypesResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
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

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class FreightCertificationReportTest extends CommonMocks {

    @InjectMocks
    private FreightCertificationReport freightCertificationReport;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IV1Service v1Service;

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
    private V1ServiceUtil v1ServiceUtil;

    @Mock
    private BillingServiceAdapter billingServiceAdapter;
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
                V1TenantSettingsResponse.builder().P100Branch(false).UseV2ScreenForBillCharges(true).DPWDateFormat("yyyy-MM-dd").GSTTaxAutoCalculation(true).EnableIGMDetails(true).build());
    }

    private void populateModel(FreightCertificationModel freightCertificationModel) {
        ShipmentModel shipmentModel = new ShipmentModel();
        shipmentModel.setId(123L);
        shipmentModel.setTransportMode(ReportConstants.SEA);
        shipmentModel.setDirection(ReportConstants.IMP);
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
        additionalDetailModel.setBLChargesDisplay("PPD");
        additionalDetailModel.setIsInland(true);
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
        freightCertificationModel.setAllContainersList(shipmentModel.getShipmentContainersList());

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
        freightCertificationModel.setShipmentDetails(shipmentModel);

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

        freightCertificationModel.setNoofpackages_word(numberToWords(freightCertificationModel.shipmentDetails.getNoOfPacks()));

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
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class)).thenReturn(parties);
        when(modelMapper.map(shipmentModel.getConsigner(), Parties.class)).thenReturn(parties2);
    }

    @Test
    void populateDictionary_BillingIntegrationDisabled() {
        FreightCertificationModel freightCertificationModel = new FreightCertificationModel();
        freightCertificationModel.setTenantDetails(new TenantModel());
        freightCertificationModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        freightCertificationModel.setUserdisplayname(UserContext.getUser().DisplayName);
        populateModel(freightCertificationModel);
        UUID randomUUID = UUID.randomUUID();
        freightCertificationModel.shipmentDetails.setGuid(randomUUID);

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.FALSE);
        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        mockTenantSettings();
        mockBill(true, false, false);
        assertNotNull(freightCertificationReport.populateDictionary(freightCertificationModel));
        mockBill(false, false, false);
        assertNotNull(freightCertificationReport.populateDictionary(freightCertificationModel));
        mockBill(true, true, false);
        assertNotNull(freightCertificationReport.populateDictionary(freightCertificationModel));
    }

    @Test
    void populateDictionary_BillingIntegrationEnabled() {
        FreightCertificationModel freightCertificationModel = new FreightCertificationModel();
        freightCertificationModel.setTenantDetails(new TenantModel());
        freightCertificationModel.setShipmentSettingsDetails(ShipmentSettingsDetailsContext.getCurrentTenantSettings());
        freightCertificationModel.setUserdisplayname(UserContext.getUser().DisplayName);
        populateModel(freightCertificationModel);
        UUID randomUUID = UUID.randomUUID();
        freightCertificationModel.shipmentDetails.setGuid(randomUUID);
        BillBaseResponse billFromBilling = new BillBaseResponse();
        billFromBilling.setGuId(randomUUID.toString());
        billFromBilling.setBillId("BIL123");
        billFromBilling.setRemarks("");

        BillChargesBaseResponse billChargesBaseResponse = new BillChargesBaseResponse();
        billChargesBaseResponse.setBillChargeCostDetails(BillChargeCostDetailsResponse.builder().build());
        billChargesBaseResponse.setBillChargeRevenueDetails(BillChargeRevenueDetailsResponse.builder()
                .currencyExchangeRateDetails(List.of(CurrencyExchangeRateDetailsResponse.builder().build()))
                .taxDetails(List.of(TaxDetailsResponse.builder().build())).build());
        billChargesBaseResponse.setChargeTypeDetails(ChargeTypeBaseResponse.builder()
                .guId(randomUUID).build());

        when(billingServiceUrlConfig.getEnableBillingIntegration()).thenReturn(Boolean.TRUE);
        when(billingServiceAdapter.fetchBill(any())).thenReturn(billFromBilling);
        when(billingServiceAdapter.fetchLastPostedInvoiceDate(any())).thenReturn(LocalDateTime.now());
        when(billingServiceAdapter.fetchBillCharges(any())).thenReturn(List.of(billChargesBaseResponse));

        mockTenantSettings();
        mockBill(true, false, true);
        assertNotNull(freightCertificationReport.populateDictionary(freightCertificationModel));
        mockBill(false, false, true);
        assertNotNull(freightCertificationReport.populateDictionary(freightCertificationModel));
        mockBill(true, true, true);
        assertNotNull(freightCertificationReport.populateDictionary(freightCertificationModel));
    }

    private void mockBill(boolean setBillData, boolean sameCurrency, boolean billingIntegrationEnabled) {
        List<BillingResponse> billingResponseList = Arrays.asList(new BillingResponse());
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(billingResponseList).build();
        if(!billingIntegrationEnabled) {
            when(v1MasterData.fetchBillingList(any())).thenReturn(dependentServiceResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class)).thenReturn(billingResponseList);
        }

        ArObjectResponse arObjectResponse = new ArObjectResponse();
        if(setBillData) {
            arObjectResponse.setInvoiceDate(LocalDateTime.now());
        }
        List<ArObjectResponse> arObjectResponseList = Arrays.asList(arObjectResponse);
        dependentServiceResponse = DependentServiceResponse.builder().data(arObjectResponseList).build();
        if(!billingIntegrationEnabled) {
            when(v1MasterData.fetchArObjectList(any())).thenReturn(dependentServiceResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), ArObjectResponse.class)).thenReturn(arObjectResponseList);
        }

        List<BillChargesResponse> billChargesResponseList = new ArrayList<>();
        BillChargesResponse billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.CHARGEABLE.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("PPD");
        billChargesResponse.setChargeTypeCode("AGENT");
        billChargesResponse.setOverseasSellCurrency("USD");
        billChargesResponse.setLocalSellAmount(BigDecimal.TEN);
        billChargesResponseList.add(billChargesResponse);

        billChargesResponse = new BillChargesResponse();
        billChargesResponse.setOverseasSellAmount(BigDecimal.TEN);
        billChargesResponse.setLocalTax(BigDecimal.TEN);
        billChargesResponse.setMeasurementBasis(Integer.toString(MeasurementBasis.CHARGEABLE.getValue()));
        billChargesResponse.setLocalCostCurrency("INR");
        billChargesResponse.setPaymentType("PPD");
        billChargesResponse.setChargeTypeCode("AGENT");
        if(sameCurrency) {
            billChargesResponse.setOverseasSellCurrency("USD");
        } else {
            billChargesResponse.setOverseasSellCurrency("INR");
        }
        billChargesResponse.setLocalSellAmount(BigDecimal.TEN);
        billChargesResponseList.add(billChargesResponse);

        dependentServiceResponse = DependentServiceResponse.builder().data(billChargesResponseList).build();
        if(!billingIntegrationEnabled) {
            when(v1MasterData.fetchBillChargesList(any())).thenReturn(dependentServiceResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class)).thenReturn(billChargesResponseList);
        }

        ChargeTypesResponse chargeTypesResponse = new ChargeTypesResponse();
        if(setBillData) {
            chargeTypesResponse.setServices("Freight");
        }
        List<ChargeTypesResponse> chargeTypesResponseList = Arrays.asList(chargeTypesResponse);
        dependentServiceResponse = DependentServiceResponse.builder().data(chargeTypesResponseList).build();
        if (!billingIntegrationEnabled) {
            when(v1MasterData.fetchChargeType(any())).thenReturn(dependentServiceResponse);
            when(jsonHelper.convertValueToList(dependentServiceResponse.getData(), ChargeTypesResponse.class)).thenReturn(chargeTypesResponseList);
        }
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
        consolidationModel.setPlaceOfIssue("Test");
        shipmentModel.setConsolidationList(Arrays.asList(consolidationModel));
        when(modelMapper.map(shipmentDetails, ShipmentModel.class)).thenReturn(shipmentModel);

        when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        DependentServiceResponse dependentServiceResponse = DependentServiceResponse.builder().data(new TenantModel()).build();
        when(v1MasterData.retrieveTenant()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(dependentServiceResponse.getData(), TenantModel.class)).thenReturn(new TenantModel());
        mockShipmentSettings();
        assertNotNull(freightCertificationReport.getDocumentModel(123L));
    }
}
