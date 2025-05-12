package com.dpw.runner.shipment.services.helper;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeDeserializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.boot.json.JsonParseException;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;


@Slf4j
public class JsonTestUtility {
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private String path = "src/test/java/com/dpw/runner/shipment/services/helper/payload.json";
    private Map<String, Object> payload;

    static {
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addDeserializer(LocalDateTime.class, new CustomLocalDateTimeDeserializer());
        objectMapper.registerModule(javaTimeModule);
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        objectMapper.configure(SerializationFeature.FAIL_ON_SELF_REFERENCES, false);
        objectMapper.configure(DeserializationFeature.FAIL_ON_MISSING_CREATOR_PROPERTIES, false);
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }
    public static ObjectMapper getMapper() {
        return objectMapper;
    }

    public JsonTestUtility() throws IOException {
        payload = objectMapper.readValue(new File(path), Map.class);
    }

    public String readJson(String fileName) throws IOException {
        return FileUtils.readFileToString(new File(fileName), StandardCharsets.UTF_8);
    }

    public String getMDMResponseString() throws JsonProcessingException {
        return convertToJson(payload.get("MDM_RESPONSE"));
    }

    public Packing getTestPacking(){
        return objectMapper.convertValue(payload.get("PACKING") , Packing.class);
    }

    public Packing getTestCsvPacking(){
        return objectMapper.convertValue(payload.get("CSV_PACKING") , Packing.class);
    }

    public VolumeWeightChargeable getVolumeWeightChargeable() { return objectMapper.convertValue(payload.get("VolumeWeightChargeable"), VolumeWeightChargeable.class); }
    public Map getMasterDataMap() {
        try {
            return objectMapper.readValue(objectMapper.writeValueAsString(payload.get("MasterDataMap")),
                    new TypeReference<Map<String, Set<String>>>() {
                    });
        }
        catch (Exception e)
        {
            return null;
        }
    }

    public Map getMasterDataMapWithCommodity() {
        try {
            return objectMapper.readValue(objectMapper.writeValueAsString(payload.get("MasterDataMapWithCommodity")),
                    new TypeReference<Map<String, Set<String>>>() {
                    });
        }
        catch (Exception e)
        {
            return null;
        }
    }

    public Map getMasterDataMapWithSameCommodity() {
        try {
            return objectMapper.readValue(objectMapper.writeValueAsString(payload.get("MasterDataMapWithSameCommodity")),
                    new TypeReference<Map<String, Set<String>>>() {
                    });
        }
        catch (Exception e)
        {
            return null;
        }
    }

    public Map getDgSubstanceContactMap() {
        try {
            return objectMapper.readValue(objectMapper.writeValueAsString(payload.get("dgSubstanceContactMap")),
                    new TypeReference<Map<Integer, Long>>() {
                    });
        }
        catch (Exception e)
        {
            return null;
        }
    }

    public Map getDgSubstanceFlashPoint() {
        try {
            return objectMapper.readValue(objectMapper.writeValueAsString(payload.get("dgSubstanceFlashPoint")),
                    new TypeReference<Map<Long, String>>() {
                    });
        }
        catch (Exception e)
        {
            return null;
        }
    }

    public PickupDeliveryDetails getTestPickupDeliveryDetails() {
        return objectMapper.convertValue(payload.get("PICKUP_DELIVERY_DETAILS"), PickupDeliveryDetails.class);
    }
    public EventsRequestV2 getTestEventsRequestV2() {
        return objectMapper.convertValue(payload.get("EVENTS_REQUEST_V2"), EventsRequestV2.class);
    }
    public PackingRequestV2 getTestPackingRequestV2(){
        return objectMapper.convertValue(payload.get("PACKING_REQUEST_V2"), PackingRequestV2.class);
    }
    public ContainerRequestV2 getTestContainerRequestV2(){
        return objectMapper.convertValue(payload.get("CONTAINER_REQUEST_V2"), ContainerRequestV2.class);
    }
    public List<Packing> getTestPackingList(){
        return convertValueToList(payload.get("PACKING_LIST"), Packing.class);
    }

    public PackSummaryResponse getTestPackSummaryResponse(){
        return objectMapper.convertValue(payload.get("PACK_SUMMARY_RESPONSE"), PackSummaryResponse.class);
    }

    public PackSummaryResponse getTestPackSummaryAirResponse(){
        return objectMapper.convertValue(payload.get("PACK_SUMMARY_RESPONSE_AIR"), PackSummaryResponse.class);
    }

    public VolumeWeightChargeable getTestVolWtChargeable(){
        return objectMapper.convertValue(payload.get("VOL_WT_CHARGEABLE"), VolumeWeightChargeable.class);
    }

    public AdditionalDetails getTestAdditionalDetails(){
        return objectMapper.convertValue(payload.get("SHIPMENT"), ShipmentDetails.class).getAdditionalDetails();
    }

    public ShipmentSettingsDetails getTestShipmentSettingsDetails() {
        return objectMapper.convertValue(payload.get("SHIPMENT_SETTINGS"), ShipmentSettingsDetails.class);
    }

    public ShipmentSettingsDetails getTestShipmentSettingsDetails_CreatePayload() {
        return objectMapper.convertValue(payload.get("SHIPMENT_SETTINGS_CREATE"), ShipmentSettingsDetails.class);
    }

    public ShipmentSettingsSyncRequest getTestShipmentSettingsSyncRequest() {
        return objectMapper.convertValue(payload.get("SHIPMENT_SETTINGS_SYNC"), ShipmentSettingsSyncRequest.class);
    }
    public List<EntityTransferMasterLists> getAutoAttachConsoleMasterData() {
        return convertValueToList(payload.get("AUTO_ATTACH_CONSOLE_MASTER_DATA_LIST"), EntityTransferMasterLists.class);
    }

    public ShipmentDetails getTestShipment() {
        ShipmentDetails shipmentDetails = objectMapper.convertValue(payload.get("NEW_SHIPMENT"), ShipmentDetails.class);
        return shipmentDetails;
    }
    public ShipmentDetails getCompleteShipment() {
        ShipmentDetails shipmentDetails = objectMapper.convertValue(payload.get("COMPLETE_SHIPMENT"), ShipmentDetails.class);
        return shipmentDetails;
    }

    public Containers getTestContainer() {
        return objectMapper.convertValue(payload.get("NEW_CONTAINER_CREATE"), Containers.class);
    }

    public ConsolidationDetails getTestConsolidation(){
        ConsolidationDetails consolidationDetails = objectMapper.convertValue(payload.get("CONSOLIDATION"), ConsolidationDetails.class);
        return consolidationDetails;
    }

    public ConsolidationDetails getTestConsolidationAir(){
        ConsolidationDetails consolidationDetails = objectMapper.convertValue(payload.get("CONSOLIDATION_AIR"), ConsolidationDetails.class);
        return consolidationDetails;
    }
    public MawbStocks getMawbStock() {
        MawbStocks mawbStocks = objectMapper.convertValue(payload.get("MAWB_STOCK"), MawbStocks.class);
        return mawbStocks;
    }
    public MawbStocksLink getNewMawbStockLink(){
        MawbStocksLink mawbStocksLink = objectMapper.convertValue(payload.get("MAWB_STOCK_LINK_NEW"), MawbStocksLink.class);
        return mawbStocksLink;
    }
    public List<Parties> getConsoldiationAddressList() {
        List<Parties> parties = convertValueToList(payload.get("CONSOLIDATION_ADDRESS_LIST"), Parties.class);
        return parties;
    }
    public Parties getParties() {
        Parties parties = objectMapper.convertValue(payload.get("PARTIES"), Parties.class);
        return parties;
    }

    public Routings getTestRouting() {
        return objectMapper.convertValue(payload.get("NEW_ROUTING"), Routings.class);
    }

    public ConsolidationDetails getTestNewConsolidation(){
        return objectMapper.convertValue(payload.get("NEW_CONSOLIDATION_CREATE"), ConsolidationDetails.class);
    }
    public AirMessagingLogs getTestAirMessagingLogs () {
        return objectMapper.convertValue(payload.get("AIR_MESSAGING_LOGS"), AirMessagingLogs.class);
    }
    public ShipmentsContainersMapping getTestShipmentsContainersMapping() {
        return objectMapper.convertValue(payload.get("SHIPMENTS_CONTAINERS_MAPPING"), ShipmentsContainersMapping.class);
    }

    public TenantProducts getTenantProducts() {
        return objectMapper.convertValue(payload.get("TENANT_PRODUCT"), TenantProducts.class);
    }

    public ConsolidationDetails getCompleteConsolidation() {
        return objectMapper.convertValue(payload.get("COMPLETE_CONSOLIDATION"), ConsolidationDetails.class);
    }

    public Awb getTestHawb() {
        Awb awb = objectMapper.convertValue(payload.get("HAWB"), Awb.class);
        return awb;
    }

    public Awb getTestDmawb() {
        Awb awb = objectMapper.convertValue(payload.get("DMAWB"), Awb.class);
        return awb;
    }

    public Awb getTestMawb() {
        Awb awb = objectMapper.convertValue(payload.get("MAWB"), Awb.class);
        return awb;
    }

    public <T> T getJson(String key, Class<T> clazz) {
        return objectMapper.convertValue(payload.get(key), clazz);
    }

    public <T,F> List<F> convertValueToList(T object, Class<F> clazz) {
        return objectMapper.convertValue(object, objectMapper.getTypeFactory().constructCollectionType(List.class, clazz));
    }

    public <T> T getCopyObject(T t, Class<T> clazz) throws JsonProcessingException {
        String json = convertToJson(t);
        return objectMapper.readValue(json, clazz);
    }

    public <T> String convertToJson(T object) {
        try {
            return objectMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            log.error("Failed Parsed Object: {}", object.toString());
            log.error("Failed to Parse given Json: " + e.getMessage());
            log.info("Exception thrown while parsing json: {}", e.toString());
            throw new JsonParseException(e);
        }
    }

    public Allocations getTestAllocation() {
        return objectMapper.convertValue(payload.get("NEW_ALLOCATION_CREATE"), Allocations.class);
    }

    public AchievedQuantities getTestAchievedQuantity() {
        return objectMapper.convertValue(payload.get("NEW_ACHIEVED_QUANTITY") , AchievedQuantities.class);
    }

    public ArrivalDepartureDetails getTestArrivalDepartureDetails() {
        return objectMapper.convertValue(payload.get("NEW_ARRIVAL_DEPART_DETAIL") , ArrivalDepartureDetails.class);
    }

    public Jobs getTestJob() {
        return objectMapper.convertValue(payload.get("NEW_JOB") , Jobs.class);
    }

    public MawbStocksLink getTestMawbStocksLink() {
        return objectMapper.convertValue(payload.get("MAWB_STOCK_LINK") , MawbStocksLink.class);
    }

    public MawbStocks getTestMawbStocks(){
        return objectMapper.convertValue(payload.get("MAWB_STOCK"), MawbStocks.class);
    }
    public MawbStocks getTestStockData() {
        return objectMapper.convertValue(payload.get("STOCK_DATA"), MawbStocks.class);
    }

    public IntegrationResponse getTestIntegrationResponse() {
        return objectMapper.convertValue(payload.get("INTEGRATION_PAYLOAD"), IntegrationResponse.class);
    }

    public ELDetails getTestELDetails() {
        return objectMapper.convertValue(payload.get("ELDETAILS") , ELDetails.class);
    }

    public Events getTestEventData() {
        return objectMapper.convertValue(payload.get("EVENT"), Events.class);
    }

    public Notes getTestNoteData() {
        return objectMapper.convertValue(payload.get("NOTE"), Notes.class);
    }

    public FileRepo getTestFileRepoData() {
        return objectMapper.convertValue(payload.get("FILE_REPO"), FileRepo.class);
    }

    public AuditLog getTestAuditLog() {
        return objectMapper.convertValue(payload.get("AUDIT_LOG"), AuditLog.class);
    }

    public BookingCarriage getTestBookingCarriage() {
        return objectMapper.convertValue(payload.get("BOOKING_CARRIAGE") , BookingCarriage.class);
    }

    public CarrierDetails getTestCarrierDetails() {
        return objectMapper.convertValue(payload.get("CARRIER_DETAILS"), CarrierDetails.class);
    }

    public DefaultViews getTestDefaultView() {
        return objectMapper.convertValue(payload.get("DEFAULT_VIEWS"), DefaultViews.class);
    }

    public OrderNumber getTestOrderNumberDao() {
        return objectMapper.convertValue(payload.get("ORDER_NUMBER"), OrderNumber.class);
    }

    public TruckDriverDetails getTestTruckDriverDetails() {
        return objectMapper.convertValue(payload.get("TRUCK_DRIVER_DETAILS"), TruckDriverDetails.class);
    }

    public ReferenceNumbers getTestReferenceNumbers() {
        return objectMapper.convertValue(payload.get("REFERENCE_NUMBERS"), ReferenceNumbers.class);
    }

    public ReportRequest getTestReportRequest() {
        ReportRequest reportRequest = objectMapper.convertValue(payload.get("REPORT_REQUEST"), ReportRequest.class);
        return reportRequest;
    }

    public CustomerBooking getCompleteCustomerBooking() {
        return objectMapper.convertValue(payload.get("COMPLETE_CUSTOMER_BOOKING"), CustomerBooking.class);
    }

    public CustomerBookingRequest getCustomerBookingRequest() {
        return objectMapper.convertValue(payload.get("CUSTOMER_BOOKING_REQUEST"), CustomerBookingRequest.class);
    }

    public CustomerBookingV3Request getCustomerBookingV3Request() {
        return objectMapper.convertValue(payload.get("CUSTOMER_BOOKING_REQUEST"), CustomerBookingV3Request.class);
    }

    public CustomerBooking getCustomerBooking() {
        return objectMapper.convertValue(payload.get("CUSTOMER_BOOKING_REQUEST"), CustomerBooking.class);
    }

    public NetworkTransfer getNetworkTransfer() {
        return objectMapper.convertValue(payload.get("NETWORK_TRANSFER_RESPONSE"), NetworkTransfer.class);
    }

    public PlatformToRunnerCustomerBookingRequest getPlatformCreateUpdateRequest() {
        return objectMapper.convertValue(payload.get("PLATFORM_CREATE_UPDATE"), PlatformToRunnerCustomerBookingRequest.class);
    }

    public EntityTransferOrganizations getOrganizationData() {
        return objectMapper.convertValue(payload.get("ORG_DATA"), EntityTransferOrganizations.class);
    }

    public EntityTransferAddress getAddressData() {
        return objectMapper.convertValue(payload.get("ADDRESS_DATA"), EntityTransferAddress.class);
    }

    public ListCommonRequest getListRequest() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest1() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST1"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest2() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST2"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequestIsEnum() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_IS_ENUM"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequestIN() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_IN"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequestIN2() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_IN2"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest4() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_4"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest5() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_5"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest6() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_6"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest7() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_7"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest8() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_8"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest9() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_9"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest10() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_10"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest11() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_11"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest12() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_12"), ListCommonRequest.class);
    }

    public ListCommonRequest getListRequest13() {
        return objectMapper.convertValue(payload.get("LIST_REQUEST_13"), ListCommonRequest.class);
    }

    public QuoteContracts getQuoteContracts() {
        return objectMapper.convertValue(payload.get("QUOTE_CONTRACTS"), QuoteContracts.class);
    }

    public ListContractResponse getListContractResponse() {
        return objectMapper.convertValue(payload.get("LIST_CONTRACT_RESPONSE"), ListContractResponse.class);
    }
    public EntityTransferShipmentDetails getImportShipmentData() {
        return objectMapper.convertValue(payload.get("ImportShipmentData"), EntityTransferShipmentDetails.class);
    }
    public EntityTransferConsolidationDetails getImportConsolidationAir() {
        return objectMapper.convertValue(payload.get("entityTransferConsolidationDetailsAir"), EntityTransferConsolidationDetails.class);
    }
    public EntityTransferConsolidationDetails getImportConsolidationSea() {
        return objectMapper.convertValue(payload.get("entityTransferConsolidationDetailsSea"), EntityTransferConsolidationDetails.class);
    }

    public Notification getNotification() {
        return objectMapper.convertValue(payload.get("NOTIFICATION_RESPONSE"), Notification.class);
    }
}
