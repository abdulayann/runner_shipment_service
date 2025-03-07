package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.DocumentService.DocumentService;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.EmailBodyResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.DocPages;
import com.dpw.runner.shipment.services.ReportingService.Models.DocUploadRequest;
import com.dpw.runner.shipment.services.ReportingService.Models.DocumentRequest;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.*;
import com.dpw.runner.shipment.services.ReportingService.ReportsFactory;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.MawbPrintFor;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.document.request.documentmanager.DocumentManagerSaveFileRequest;
import com.dpw.runner.shipment.services.document.response.DocumentManagerDataResponse;
import com.dpw.runner.shipment.services.document.response.DocumentManagerResponse;
import com.dpw.runner.shipment.services.document.service.IDocumentManagerService;
import com.dpw.runner.shipment.services.document.util.BASE64DecodedMultipartFile;
import com.dpw.runner.shipment.services.dto.request.CustomAutoEventRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.ReportRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.ReportException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.notification.request.TagsData;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.IReportService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.google.common.base.Strings;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Image;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TENANTID;
import static com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants.GUID;

@Service
@Slf4j
public class ReportService implements IReportService {

    @Autowired
    private HblReport hblReport;

    @Autowired
    private ReportsFactory reportsFactory;

    @Autowired
    private DocumentService documentService;

    @Autowired
    private JsonHelper jsonHelper;
    
    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IHblTermsConditionTemplateDao hblTermsConditionTemplateDao;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IEventService eventService;

    @Autowired
    private IHblDao hblDao;

    @Autowired
    private IDocumentManagerService documentManagerService;

    @Autowired
    private IShipmentService shipmentService;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IShipmentSync shipmentSync;
    @Autowired
    private IHblReleaseTypeMappingDao hblReleaseTypeMappingDao;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    ExecutorService executorService;
    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private AWBLabelReport awbLabelReport;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;
    @Autowired
    private IDpsEventService dpsEventService;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ConsolidationService consolidationService;

    @Autowired
    private IV1Service iv1Service;

    @Autowired
    private IDocDetailsDao docDetailsDao;

    @Autowired
    @Lazy
    private ShipmentTagsForExteranlServices shipmentTagsForExteranlServices;

    private static final int MAX_BUFFER_SIZE = 10 * 1024;

    @Override
    @Transactional
    public byte[] getDocumentData(CommonRequestModel request)
        throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException {
        ReportRequest reportRequest = (ReportRequest) request.getData();

        // Generate combined shipment report via consolidation
        if((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.CARGO_MANIFEST) || reportRequest.getReportInfo().equalsIgnoreCase( ReportConstants.SHIPMENT_CAN_DOCUMENT) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SHIPPING_INSTRUCTION)) && reportRequest.isFromConsolidation()) {
            Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(Long.valueOf(reportRequest.getReportId()));
            if(optionalConsolidationDetails.isPresent()) {
                ConsolidationDetails consolidationDetails = optionalConsolidationDetails.get();
                byte[] dataByte;
                List<byte[]> dataByteList = new ArrayList<>();
                for(ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                    reportRequest.setFromConsolidation(false);
                    reportRequest.setReportId(shipmentDetails.getId().toString());
                    dataByte = getDocumentData(CommonRequestModel.buildRequest(reportRequest));
                    if(dataByte != null) {
                        dataByteList.add(dataByte);
                    }
                }
                return CommonUtils.concatAndAddContent(dataByteList);
            }
        }

        // if report info is CargoManifestAirExportShipment check original awb printed before
        if(Objects.equals(reportRequest.getReportInfo(), ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT)) {
            Long shipmentId = Long.valueOf(reportRequest.getReportId());
            var awbList = awbDao.findByShipmentId(shipmentId);
        }

        // CargoManifestAirExportConsolidation , validate original awb printed for its HAWB
        if(Objects.equals(reportRequest.getReportInfo(), ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION)) {
            Long consolidationId = Long.valueOf(reportRequest.getReportId());
            var awbList = awbDao.findByConsolidationId(consolidationId);
            if(awbList != null && !awbList.isEmpty()) {
                List<Awb> linkedHawb = awbDao.getLinkedAwbFromMawb(awbList.get(0).getId());
                long count = linkedHawb.stream().filter(i -> !Objects.equals(PrintType.ORIGINAL_PRINTED, i.getPrintType())).count();

                if(count > 0) {
                    throw new RunnerException("Please print original AWB for linked shipments before proceeding !");
                }
            }
        }

        if((Objects.equals(reportRequest.getReportInfo(), ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION)
                || Objects.equals(reportRequest.getReportInfo(), ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION))
                && reportRequest.isFromConsolidation()) {
            Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(Long.valueOf(reportRequest.getReportId()));
            if(optionalConsolidationDetails.isPresent()) {
                ConsolidationDetails consolidationDetails = optionalConsolidationDetails.get();
                byte[] dataByte;
                List<byte[]> dataByteList = new ArrayList<>();
                Map<String, List<Long>> groupedShipments = new HashMap<>();
                if(consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
                    groupedShipments = consolidationDetails.getShipmentsList().stream()
                            .collect(Collectors.groupingBy(e -> e.getCarrierDetails().getDestinationPort(),
                                    Collectors.mapping(ShipmentDetails::getId, Collectors.toList())));
                    if(groupedShipments != null && !groupedShipments.isEmpty()) {
                        for (Map.Entry<String, List<Long>> entry: groupedShipments.entrySet()) {
                            reportRequest.setFromConsolidation(false);
                            reportRequest.setShipmentIds(entry.getValue());
                            dataByte = getDocumentData(CommonRequestModel.buildRequest(reportRequest));
                            if(dataByte != null) {
                                dataByteList.add(dataByte);
                            }
                        }
                    }

                }
                return CommonUtils.concatAndAddContent(dataByteList);
            }
        }

        ShipmentSettingsDetails tenantSettingsRow = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant()).orElse(ShipmentSettingsDetails.builder().build());

        Boolean isOriginalPrint = false;
        Boolean isSurrenderPrint = false;
        Boolean isNeutralPrint = false;
        if(StringUtility.isNotEmpty(reportRequest.getPrintType())){
            isOriginalPrint = reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL);
            isSurrenderPrint = reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.SURRENDER);
            isNeutralPrint = reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.NEUTRAL);
        }

        IReport report = reportsFactory.getReport(reportRequest.getReportInfo());
        if (report instanceof MawbReport mawbReport) {
            mawbReport.isDMawb = reportRequest.isFromShipment(); // Set isDMawb based on isFromShipment flag
            mawbReport.printType = reportRequest.getPrintType();

            if (!reportRequest.isFromShipment()) { // Case: Request came from consolidation
                long consolidationId = Long.parseLong(reportRequest.getReportId());
                List<Long> shipmentIdsList = consoleShipmentMappingDao.findByConsolidationId(consolidationId)
                        .stream().map(ConsoleShipmentMapping::getShipmentId).toList(); // Extract shipment IDs

                // Check if DPS implication(MAWBPR) is present for any shipment
                if (!shipmentIdsList.isEmpty() && Boolean.TRUE.equals(dpsEventService.isImplicationPresent(shipmentIdsList, DpsConstants.MAWBPR))) {
                    throw new ReportException(DpsConstants.DPS_ERROR_1);
                }
            } else if (Boolean.TRUE.equals(isOriginalPrint)) { // Case: Request came from shipment and is an original print
                long shipmentId = Long.parseLong(reportRequest.getReportId());
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId)
                        .orElseThrow(() -> new ValidationException("No Shipment found with Id: " + shipmentId));

                // Check if the shipment type is DRT and has a DPS implication (MAWBPR)
                if (Constants.SHIPMENT_TYPE_DRT.equals(shipmentDetails.getJobType()) &&
                        Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(shipmentId), DpsConstants.MAWBPR))) {
                    throw new ReportException(DpsConstants.DPS_ERROR_1); // Throw error if implication is found
                }
            }
        }

        if(Boolean.TRUE.equals(reportRequest.getPushAwbEvent()) && reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && Boolean.TRUE.equals(isOriginalPrint)) {
            awbDao.airMessagingIntegration(Long.parseLong(reportRequest.getReportId()), reportRequest.getReportInfo(), reportRequest.isFromShipment(), reportRequest.isIncludeCsdInfo());
        } else if((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) && Boolean.TRUE.equals(isOriginalPrint)) {
            if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && !reportRequest.isFromShipment())
                awbDao.updateAirMessageStatusFromConsolidationId(Long.parseLong(reportRequest.getReportId()), AwbStatus.AWB_ORIGINAL_PRINTED.name());
            else
                awbDao.updateAirMessageStatusFromShipmentId(Long.parseLong(reportRequest.getReportId()), AwbStatus.AWB_ORIGINAL_PRINTED.name());
        }

        // Awb print status set for Hawb and Mawb
        this.setPrintTypeForAwb(reportRequest, isOriginalPrint);

        boolean reportingNewFlow = false;
        Map<String, Object> dataRetrived = new HashMap<>();
        boolean newFlowSuccess = false;

        if(report instanceof AWBLabelReport awbLabelReport) {
            awbLabelReport.setMawb(reportRequest.isFromConsolidation());
            awbLabelReport.setRemarks(reportRequest.getRemarks());
            awbLabelReport.setCombi(reportRequest.isCombiLabel());
            awbLabelReport.setCustomLabel(reportRequest.isCombiLabel());
        }
        if(report instanceof FCRDocumentReport fcrDocumentReport) {
            fcrDocumentReport.setFcrShipper(reportRequest.getFcrShipper());
            fcrDocumentReport.setPackIds(reportRequest.getPackIds());
            fcrDocumentReport.setIssueDate(reportRequest.getDateOfIssue());
            fcrDocumentReport.setPlaceOfIssue(reportRequest.getPlaceOfIssue());
        }
        // user story 135668
        if(report instanceof ArrivalNoticeReport) {
            ((ArrivalNoticeReport) report).printWithoutTranslation = reportRequest.getPrintWithoutTranslation();
        }
        if(report instanceof BookingConfirmationReport) {
            ((BookingConfirmationReport) report).printWithoutTranslation = reportRequest.getPrintWithoutTranslation();
        }
        if(report instanceof PickupOrderReport) {
            ((PickupOrderReport) report).printWithoutTranslation = reportRequest.getPrintWithoutTranslation();
        }
        if(report instanceof DeliveryOrderReport) {
            ((DeliveryOrderReport) report).printWithoutTranslation = reportRequest.getPrintWithoutTranslation();
        }
        if(report instanceof PreAlertReport) {
            ((PreAlertReport) report).printWithoutTranslation = reportRequest.getPrintWithoutTranslation();
        }
        if(report instanceof ShipmentCANReport) {
            ((ShipmentCANReport) report).printWithoutTranslation = reportRequest.getPrintWithoutTranslation();
        }
        if(report instanceof CargoManifestAirConsolidationReport cargoManifestAirConsolidationReport) {
            cargoManifestAirConsolidationReport.setShipIds(reportRequest.getShipmentIds());
            cargoManifestAirConsolidationReport.setShipperAndConsignee(reportRequest.isShipperAndConsignee());
            cargoManifestAirConsolidationReport.setSecurityData(reportRequest.isSecurityData());
        }
        if(report instanceof CargoManifestAirShipmentReport cargoManifestAirShipmentReport) {
            cargoManifestAirShipmentReport.setShipperAndConsignee(reportRequest.isShipperAndConsignee());
            cargoManifestAirShipmentReport.setSecurityData(reportRequest.isSecurityData());
        }

        if(report instanceof CSDReport csdReport) {
            csdReport.setIsConsolidation(reportRequest.isFromConsolidation());
        }

//        if (reportingNewFlow || ReportConstants.NEW_TEMPLATE_FLOW.contains(reportRequest.getReportInfo())) {
//            try {
//                //dataRetrived = new ReportRepository().getReportDataNewFlow(ReportInfo, ReportId);
//                newFlowSuccess = true;
//            } catch (Exception ignored) {
//                dataRetrived = null;
//            }
//        }

        //TODO - Need to handle for new flow
        if (report instanceof PickupOrderReport pickupOrderReport && StringUtility.isNotEmpty(reportRequest.getTransportInstructionId())) {
            dataRetrived = pickupOrderReport.getData(Long.parseLong(reportRequest.getReportId()), Long.parseLong(reportRequest.getTransportInstructionId()));
        } else if (report instanceof DeliveryOrderReport vDeliveryOrderReport) {
            // Verify if the specified implication (DOPR) exists for the report's ID.
            // If true, throw a ReportException indicating the implication is already present.
            if (Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(Long.parseLong(reportRequest.getReportId())), DpsConstants.DOPR))) {
                throw new ReportException(DpsConstants.DPS_ERROR_1);
            }

            // If a Transport Instruction ID is provided in the request:
            if (StringUtility.isNotEmpty(reportRequest.getTransportInstructionId())) {
                // Retrieve data using both the Report ID and Transport Instruction ID.
                dataRetrived = vDeliveryOrderReport.getData(
                        Long.parseLong(reportRequest.getReportId()),
                        Long.parseLong(reportRequest.getTransportInstructionId()));
            } else {
                // If no Transport Instruction ID is provided, retrieve data using only the Report ID.
                dataRetrived = report.getData(Long.parseLong(reportRequest.getReportId()));
            }

            // Create an event for the report request with the specific event constant DOGE.
            createEvent(reportRequest, EventConstants.DOGE);
        } else if (report instanceof TransportOrderReport transportOrderReport && StringUtility.isNotEmpty(reportRequest.getTransportInstructionId())) {
            dataRetrived = transportOrderReport.getData(Long.parseLong(reportRequest.getReportId()), Long.parseLong(reportRequest.getTransportInstructionId()));
        } else if (report instanceof HblReport vHblReport) {
            if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL)) {

                // Verify if the specified implication (HBLPR) exists for the report's ID.
                // If true, throw a ReportException indicating the implication is already present.
                if (Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(Long.parseLong(reportRequest.getReportId())), DpsConstants.HBLPR))) {
                    throw new ReportException(DpsConstants.DPS_ERROR_1);
                }

                dataRetrived = vHblReport.getData(Long.parseLong(reportRequest.getReportId()), ReportConstants.ORIGINAL);
                createEvent(reportRequest, EventConstants.FHBL);
            } else if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)) {
                dataRetrived = vHblReport.getData(Long.parseLong(reportRequest.getReportId()), ReportConstants.DRAFT);
                createEvent(reportRequest, EventConstants.DHBL);
            } else {
                dataRetrived = report.getData(Long.parseLong(reportRequest.getReportId()));
            }
        } else if (report instanceof PreAlertReport vPreAlertReport) {
            dataRetrived = vPreAlertReport.getData(Long.parseLong(reportRequest.getReportId()));
            createEvent(reportRequest, EventConstants.PRST);
        } else if (report instanceof HawbReport vHawbReport && reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL)) {

            // Verify if the specified implication (HAWBPR) exists for the report's ID.
            // If true, throw a ReportException indicating the implication is already present.
            if (Boolean.TRUE.equals(dpsEventService.isImplicationPresent(List.of(Long.parseLong(reportRequest.getReportId())), DpsConstants.HAWBPR))) {
                throw new ReportException(DpsConstants.DPS_ERROR_1);
            }
            vHawbReport.printType = reportRequest.getPrintType();

            dataRetrived = vHawbReport.getData(Long.parseLong(reportRequest.getReportId()));
            createEvent(reportRequest, EventConstants.HAWB);
        } else if (report instanceof BookingConfirmationReport vBookingConfirmationReport) {
            dataRetrived = vBookingConfirmationReport.getData(Long.parseLong(reportRequest.getReportId()));
        } else if (report instanceof SeawayBillReport vSeawayBillReport) {
            dataRetrived = vSeawayBillReport.getData(Long.parseLong(reportRequest.getReportId()));
            createEvent(reportRequest, EventConstants.FHBL);
        } else if (report instanceof HawbReport vHawbReport) {
            vHawbReport.printType = reportRequest.getPrintType();
            dataRetrived = vHawbReport.getData(Long.parseLong(reportRequest.getReportId()));
        } else {
            dataRetrived = report.getData(Long.parseLong(reportRequest.getReportId()));
        }

        boolean isOriginalPrinted = (boolean) dataRetrived.getOrDefault(ReportConstants.PRINTED_ORIGINAL, false);
        String hbltype = (String)dataRetrived.getOrDefault(ReportConstants.HOUSE_BILL_TYPE, null);
        String objectType = "";
        if (ReportConstants.OBJECT_TYPE_REPORTS.contains(reportRequest.getReportInfo()))
        {
            if (dataRetrived.containsKey(ReportConstants.OBJECT_TYPE))
            {
                objectType = dataRetrived.get(ReportConstants.OBJECT_TYPE).toString();
            }
        }
        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.AWB_LABEL)){
            List<byte[]> pdf_Bytes = new ArrayList<>();
            DocPages pages = GetFromTenantSettings(reportRequest.getReportInfo(), null, null, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), false, null, null,false);
            generatePdfBytes(reportRequest, pages, dataRetrived, pdf_Bytes);
            return CommonUtils.concatAndAddContent(pdf_Bytes);
        }
        else if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB))
        {

            if (reportRequest.isPrintIATAChargeCode())
            {
                dataRetrived.put(ReportConstants.OTHER_CHARGES, dataRetrived.get(ReportConstants.OTHER_CHARGES_IATA));
                dataRetrived.put(ReportConstants.NEW_OTHER_CHARGES, dataRetrived.get(ReportConstants.NEW_OTHER_CHARGES_IATA));
            } else {
                dataRetrived.remove(ReportConstants.OTHER_CHARGES_IATA);
            }

            if(!reportRequest.isPrintCSD()){
                dataRetrived.remove(RA_CSD);
            }

            if(reportRequest.getDisplayFreightAmount()!=null && !reportRequest.getDisplayFreightAmount())
            {
                dataRetrived.put(ReportConstants.PACKING_LIST, dataRetrived.get(ReportConstants.PACKING_LIST_FAT));
                dataRetrived.put(ReportConstants.SUM_OF_TOTAL_AMOUNT, StringUtility.getEmptyString());
                dataRetrived.put(ReportConstants.WT_CHARGE_P, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.WT_CHARGE_C, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_C));
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_OTHERS_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_OTHERS_C));
                dataRetrived.put(ReportConstants.VALUATION_CHARGES_C, ReportConstants.AS_AGREED_DISPLAY);
                dataRetrived.put(ReportConstants.VALUATION_CHARGES_P, ReportConstants.AS_AGREED_DISPLAY);
                dataRetrived.put(ReportConstants.TAX_C, ReportConstants.AS_AGREED_DISPLAY);
                dataRetrived.put(ReportConstants.TAX_P, ReportConstants.AS_AGREED_DISPLAY);
            }
            if (reportRequest.getDisplayOtherAmount() !=null && !reportRequest.getDisplayOtherAmount())
            {
                List<String> otherCharges = new ArrayList<>();
                otherCharges.add(dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT).toString());
                dataRetrived.put(ReportConstants.OTHER_CHARGES, otherCharges);
                dataRetrived.put(ReportConstants.NEW_OTHER_CHARGES,otherCharges);
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_FREIGHT_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_FREIGHT_C));
                dataRetrived.put(ReportConstants.AGENT_DUE_P, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.CARRIER_DUE_P, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.AGENT_DUE_C, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_C));
                dataRetrived.put(ReportConstants.CARRIER_DUE_C, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_C));
            }
            if(reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && reportRequest.getDisplayOtherAmount())
            {
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_OTHERS_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_OTHERS_C));
            }
            if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && !reportRequest.getDisplayOtherAmount())
            {
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_C));
            }
            List<byte[]> pdf_Bytes = new ArrayList<>();
            if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.NEUTRAL)) {
                return getBytesForNeutralAWB(dataRetrived);
            }
            DocPages Pages = GetFromTenantSettings(reportRequest.getReportInfo(), null, null, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), false, null, null, false);
            byte[] pdfByte_Content = null;
            if(reportRequest.isPrintForParties()){
                pdfByte_Content = printForPartiesAndBarcode(reportRequest, pdf_Bytes, dataRetrived.get(ReportConstants.MAWB_NUMBER) == null ? "": dataRetrived.get(ReportConstants.MAWB_NUMBER).toString(), dataRetrived, Pages);
            }else{
                pdfByte_Content = GetFromDocumentService(dataRetrived, Pages.getMainPageId());
                if(pdfByte_Content == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
            }
            var shc = dataRetrived.getOrDefault(ReportConstants.SPECIAL_HANDLING_CODE, null);
            boolean addWaterMarkForEaw = false;
            if(shc != null){
                Pattern pattern = Pattern.compile("\\s*,\\s*");
                List<String> items = Arrays.asList(pattern.split(shc.toString()));
                if(!items.isEmpty() && items.contains(Constants.EAW)){
                    addWaterMarkForEaw = true;
                }
            }
            if(addWaterMarkForEaw && reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Draft.name())) {
                pdfByte_Content = CommonUtils.addWatermarkToPdfBytes(pdfByte_Content, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_EAW_WATERMARK);
            }
            else if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)){
                pdfByte_Content = CommonUtils.addWatermarkToPdfBytes(pdfByte_Content, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_WATERMARK);
            } else if(addWaterMarkForEaw && Boolean.TRUE.equals(isOriginalPrint)) {
                pdfByte_Content = CommonUtils.addWatermarkToPdfBytes(pdfByte_Content, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.ORIGINAL_EAW_WATERMARK);
            }
            //Update shipment issue date
            if ((isOriginalPrint || isSurrenderPrint) && reportRequest.getReportKey() != null && reportRequest.getReportKey().equalsIgnoreCase(ReportConstants.SHIPMENT_ID))
            {
                shipmentService.updateDateAndStatus(Long.parseLong(reportRequest.getReportId()), LocalDate.now().atStartOfDay(), null);
            }

            addDocumentToDocumentMaster(reportRequest, pdfByte_Content);

            triggerAutomaticTransfer(report, reportRequest);

            return pdfByte_Content;
        }
        else if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB))
        {
            if (!reportRequest.isPrintIATAChargeCode()) {
                dataRetrived.remove(ReportConstants.OTHER_CHARGES_IATA);
            }
            if(!reportRequest.isPrintCSD()){
                dataRetrived.remove(RA_CSD);
            }
            if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount())
            {
                dataRetrived.put(ReportConstants.PACKING_LIST, dataRetrived.get(ReportConstants.PACKING_LIST_FAT));
                dataRetrived.put(ReportConstants.SUM_OF_TOTAL_AMOUNT, StringUtility.getEmptyString());
                dataRetrived.put(ReportConstants.WT_CHARGE_P, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.WT_CHARGE_C, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_C));
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_OTHERS_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_OTHERS_C));
                dataRetrived.put(ReportConstants.VALUATION_CHARGES_C, ReportConstants.AS_AGREED_DISPLAY);
                dataRetrived.put(ReportConstants.VALUATION_CHARGES_P, ReportConstants.AS_AGREED_DISPLAY);
                dataRetrived.put(ReportConstants.TAX_C, ReportConstants.AS_AGREED_DISPLAY);
                dataRetrived.put(ReportConstants.TAX_P, ReportConstants.AS_AGREED_DISPLAY);
            }
            if (reportRequest.getDisplayOtherAmount() != null && !reportRequest.getDisplayOtherAmount())
            {
                List<String> otherCharges = new ArrayList<>();
                otherCharges.add(dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT).toString());
                dataRetrived.put(ReportConstants.OTHER_CHARGES, otherCharges);
                dataRetrived.put(ReportConstants.NEW_OTHER_CHARGES, otherCharges);
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_FREIGHT_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_FREIGHT_C));
                dataRetrived.put(ReportConstants.AGENT_DUE_P, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.CARRIER_DUE_P, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.AGENT_DUE_C, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_C));
                dataRetrived.put(ReportConstants.CARRIER_DUE_C, dataRetrived.get(ReportConstants.OTHER_AMOUNT_TEXT_C));
            }
            if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && reportRequest.getDisplayOtherAmount())
            {
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_OTHERS_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_OTHERS_C));
            }
            if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && !reportRequest.getDisplayOtherAmount())
            {
                dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_P));
                dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_C));
            }

            if (isOriginalPrint || isSurrenderPrint || isNeutralPrint)
            {
                LocalDateTime issueDate = null;
                ShipmentStatus status = null;
                if (ReportConstants.AIR.equalsIgnoreCase(dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString()) && (isOriginalPrint || isNeutralPrint))
                {
                    status = ShipmentStatus.GenerateHAWB;
                    if (isOriginalPrint || isSurrenderPrint)
                    {
                        issueDate = LocalDate.now().atStartOfDay();
                    }
                }
                else if (ReportConstants.SEA.equalsIgnoreCase(dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString()) && (isOriginalPrint || isSurrenderPrint))
                {
                    status = ShipmentStatus.GenerateHBL;

                }
                shipmentService.updateDateAndStatus(Long.parseLong(reportRequest.getReportId()), issueDate, status.getValue());
            }

            List<byte[]> pdf_Bytes = new ArrayList<>();
            if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.NEUTRAL))
                return getBytesForNeutralAWB(dataRetrived);

            DocPages Pages = GetFromTenantSettings(reportRequest.getReportInfo(), hbltype, objectType, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), isOriginalPrinted, reportRequest.getTransportMode(), reportRequest.getMultiTemplateCode(),false);
            byte[] pdfByte_Content = null;
            byte[] mainDocHawb = null;
            Map<String, Object> dataRetrived1 = dataRetrived;
            CompletableFuture<byte[]> mainDocFuture = null;
            boolean asyncFlag = Boolean.FALSE;
            if(reportRequest.isPrintForParties()){
                mainDocHawb = printForPartiesAndBarcode(reportRequest, pdf_Bytes, dataRetrived.get(ReportConstants.HAWB_NO) == null? "" : dataRetrived.get(ReportConstants.HAWB_NO).toString(), dataRetrived, Pages);
            }else{
                asyncFlag = Boolean.TRUE;
                mainDocFuture = CompletableFuture.supplyAsync(
                    () -> GetFromDocumentService(dataRetrived1, Pages.getMainPageId()),
                    executorService);
            }
           var firstPageHawbFuture =  CompletableFuture.supplyAsync(
                () -> GetFromDocumentService(dataRetrived1, Pages.getFirstPageId()),
                executorService);
            var backPageHawbFuture =  CompletableFuture.supplyAsync(
                () -> GetFromDocumentService(dataRetrived1, Pages.getBackPrintId()),
                executorService);
            if (asyncFlag) {
                CompletableFuture.allOf(mainDocFuture, firstPageHawbFuture, backPageHawbFuture)
                    .join();
                mainDocHawb = mainDocFuture.get();
            } else {
                CompletableFuture.allOf(firstPageHawbFuture, backPageHawbFuture)
                    .join();
            }

            byte[] firstPageHawb = firstPageHawbFuture.get();
            byte[] backPrintHawb = backPageHawbFuture.get();
            if (mainDocHawb == null)
            {
                throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
            }
            List<byte[]> pdfBytesHawb = getOriginalandCopies(Pages, reportRequest.getReportInfo(), mainDocHawb, firstPageHawb, backPrintHawb, dataRetrived, hbltype, tenantSettingsRow, reportRequest.getNoOfCopies(), reportRequest);
            pdfByte_Content = CommonUtils.concatAndAddContent(pdfBytesHawb);
            if (pdfByte_Content == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
            var shc = dataRetrived.getOrDefault(ReportConstants.SPECIAL_HANDLING_CODE, null);
            Boolean addWaterMarkForEaw = false;
            if(shc != null){
                List<String> items = Arrays.asList(shc.toString().split("\\s*,\\s*"));
                if(!items.isEmpty() && items.contains(Constants.EAW)){
                    addWaterMarkForEaw = true;
                }
            }
            if(addWaterMarkForEaw && reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Draft.name())) {
                pdfByte_Content = CommonUtils.addWatermarkToPdfBytes(pdfByte_Content, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_EAW_WATERMARK);
            }
            else if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Draft.name()))
            {
                pdfByte_Content = CommonUtils.addWatermarkToPdfBytes(pdfByte_Content, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_WATERMARK);
            } else if(addWaterMarkForEaw && Boolean.TRUE.equals(isOriginalPrint)) {
                pdfByte_Content = CommonUtils.addWatermarkToPdfBytes(pdfByte_Content, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.ORIGINAL_EAW_WATERMARK);
            }

            addDocumentToDocumentMaster(reportRequest, pdfByte_Content);

            triggerAutomaticTransfer(report, reportRequest);

            //Update shipment issue date
            return pdfByte_Content;
        }
        else if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.BOOKING_ORDER)) {
            String consolidationType = dataRetrived.get(ReportConstants.SHIPMENT_TYPE) != null ?
                dataRetrived.get(ReportConstants.SHIPMENT_TYPE).toString() : null;
            String transportMode = ReportConstants.SEA;

            if (dataRetrived.containsKey(ReportConstants.TRANSPORT_MODE)){
                transportMode = dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString();
            }

            DocPages pages = GetFromTenantSettings(reportRequest.getReportInfo(), null, consolidationType, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), isOriginalPrinted, transportMode, reportRequest.getMultiTemplateCode(),false);

            byte[] pdfByte_Content = GetFromDocumentService(dataRetrived, pages.getMainPageId());
            if(pdfByte_Content == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);

            return pdfByte_Content;
        }

//        Long id = (Long) dataRetrived.getOrDefault(ReportConstants.ID, null); TODO- Removed this code for now, not in use

        if (StringUtility.isNotEmpty(reportRequest.getPrintType()))
        {
            String documentPrintType = "ZERO (0)"; //Draft
            if (reportRequest.getPrintType().equalsIgnoreCase("ORIGINAL"))
            {
                documentPrintType = "THREE (3)"; //Original
            }
            else if(reportRequest.getPrintType().equalsIgnoreCase("SURRENDER"))
            {
                documentPrintType = "ONE (1)"; //Surrender
            }

           dataRetrived.put(ReportConstants.DOCUMENT_PRINT_TYPE, documentPrintType);
        }

//        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.PACKING_LIST) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.FREIGHT_CERTIFICATION) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.PRE_ALERT) ||
//                reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.BOOKING_CONFIRMATION) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.PICKUP_ORDER) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.DELIVERY_ORDER) ||
//                reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SHIPMENT_CAN_DOCUMENT) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.COMMERCIAL_INVOICE) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.CUSTOMS_INSTRUCTION) ||
//                reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.ARRIVAL_NOTICE) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.EX)){
//
//
//        }

        if (dataRetrived.containsKey(ReportConstants.TRANSPORT_MODE)){
            objectType = dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString();
        }

        DocPages pages = GetFromTenantSettings(reportRequest.getReportInfo(), hbltype, objectType, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), isOriginalPrinted, reportRequest.getTransportMode(), reportRequest.getMultiTemplateCode(), StringUtility.isNotEmpty(reportRequest.getTransportInstructionId()));
        if (pages == null)
        {
            return null;
        }
        Map<String, Object> retrived = dataRetrived;
        var mainDocFuture = CompletableFuture.supplyAsync(() -> GetFromDocumentService(retrived, pages.getMainPageId()), executorService);
        var firstPageFuture = CompletableFuture.supplyAsync(() -> GetFromDocumentService(retrived, pages.getFirstPageId()), executorService);
        var backPrintFuture = CompletableFuture.supplyAsync(() -> GetFromDocumentService(retrived, pages.getBackPrintId()), executorService);
        CompletableFuture.allOf(mainDocFuture, firstPageFuture, backPrintFuture).join();

        byte[] mainDoc = mainDocFuture.get();
        byte[] firstpage = firstPageFuture.get();
        byte[] backprint = backPrintFuture.get();
        byte[] pdfByteContent;
        if (mainDoc == null)
        {
            throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
        }

        List<byte[]> pdfBytes = getOriginalandCopies(pages, reportRequest.getReportInfo(), mainDoc, firstpage, backprint, dataRetrived, hbltype, tenantSettingsRow, reportRequest.getNoOfCopies(), reportRequest);
        boolean waterMarkRequired = true;
        try
        {
            boolean fWaterMark = false;
            boolean bWaterMark = false;
            if(reportRequest.getFrontTemplateCode() != null || reportRequest.getBackTemplateCode() != null) {
                if (reportRequest.getFrontTemplateCode() != null) {
                    HblTermsConditionTemplate frontTemplate = hblTermsConditionTemplateDao.getTemplateCode(reportRequest.getFrontTemplateCode(), true, reportRequest.getPrintType());
                    if (frontTemplate != null) {
                        fWaterMark = frontTemplate.getIsWaterMarkRequired();
                    }
                }
                if (reportRequest.getBackTemplateCode() != null) {
                    HblTermsConditionTemplate backTemplate = hblTermsConditionTemplateDao.getTemplateCode(reportRequest.getBackTemplateCode(), false, reportRequest.getPrintType());
                    if (backTemplate != null) {
                        bWaterMark = backTemplate.getIsWaterMarkRequired();
                    }
                }
                waterMarkRequired = fWaterMark && bWaterMark;
            }
        }
        catch (ValidationException ex)
        {
            waterMarkRequired = true;
        }

        pdfByteContent = CommonUtils.concatAndAddContent(pdfBytes);
        BaseFont font = BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED);

        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HOUSE_BILL))
        {
            boolean disableOriginal = (Boolean)dataRetrived.getOrDefault(ReportConstants.DISABLE_ORIGINAL, false);
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(Long.parseLong(reportRequest.getReportId()));
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent()) {
                shipmentDetails = shipmentsRow.get();
            }
            if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL) && disableOriginal) {
                throw new ValidationException("HBl Original generation is disabled");
            }
            if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT))
            {
                if (waterMarkRequired)
                {
                    pdfByteContent = CommonUtils.addWatermarkToPdfBytes(pdfByteContent, font, ReportConstants.DRAFT_WATERMARK);
                }
                shipmentDetails.getAdditionalDetails().setDraftPrinted(true);
            }
            if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.eHBL.name()))
            {
                if (waterMarkRequired)
                {
                    pdfByteContent = CommonUtils.addWatermarkToPdfBytes(pdfByteContent, font, "NOT ORIGINAL");
                }
                shipmentDetails.getAdditionalDetails().setWBLPrinted(true);
            }
            if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Surrender.name()))
            {
                if (waterMarkRequired)
                {
                    pdfByteContent = CommonUtils.addWatermarkToPdfBytes(pdfByteContent, font, "SURRENDER");
                }
                shipmentDetails.getAdditionalDetails().setSurrenderPrinted(true);
            }
            if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Original.name()))
            {
                shipmentDetails.getAdditionalDetails().setPrintedOriginal(true);
            }

            //TODO - Need to implement
//                if ((printType.ToUpper() == TypeOfHBLPrint.Surrender.GetName().ToUpper() || printType.ToUpper() == "ORIGINAL" ) && shipmentsRow.IntegrationTableId!=null)
//                {
//                    string integrationId = shipmentsRow.IntegrationTableId;
//                    string hbl =  shipmentsRow.ShipmentType == null ? shipmentsRow.HouseBill : shipmentsRow.ShipmentType.ToUpper().Equals("DRT") ? shipmentsRow.MasterBill : shipmentsRow.HouseBill;
//                    var query = new SqlUpdate("IntegrationTable").Set("Ref3", hbl)
//                            .Where(new Criteria("Id") == integrationId).Execute(connection, ExpectedRows.Ignore);
//                }

            if (isOriginalPrint || isSurrenderPrint || isNeutralPrint) {
                if(isOriginalPrint){
                    shipmentDetails.getAdditionalDetails().setPrintedOriginal(true);
                }
                if(ReportConstants.AIR.equalsIgnoreCase(dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString()) && (isOriginalPrint || isNeutralPrint)){
                    shipmentDetails.setStatus(ShipmentStatus.GenerateHAWB.getValue());
                }else if(ReportConstants.SEA.equalsIgnoreCase(dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString()) && (isOriginalPrint || isSurrenderPrint)){
                    shipmentDetails.setStatus(ShipmentStatus.GenerateHBL.getValue());
                }
            }

            //Update shipment issue date

            //TODO - Commenting As per Product confirmation
//            if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Original.name()) || reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Surrender.name()))
//            {
//                shipmentDetails.getAdditionalDetails().setDateOfIssue(LocalDate.now().atStartOfDay());
//            }
            shipmentDetails = shipmentDao.update(shipmentDetails, false);
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, Optional.ofNullable(shipmentDetails).map(ShipmentDetails::getContainersList).orElse(null));
            try {
                shipmentSync.sync(shipmentDetails, null, null, UUID.randomUUID().toString(), false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
            if (pdfByteContent != null)
            {
                String documentType = ReportConstants.SHIPMENT_HOUSE_BILL;
                if(reportRequest.getPrintType().equalsIgnoreCase("ORIGINAL")) {
                    documentType = ReportConstants.ORIGINAL_HOUSE_BILL;
                } else if(reportRequest.getPrintType().equalsIgnoreCase("DRAFT")) {
                    documentType = ReportConstants.DRAFT_HOUSE_BILL;
                }
                DocUploadRequest docUploadRequest = new DocUploadRequest();
                docUploadRequest.setEntityType(Constants.Shipments);
                docUploadRequest.setId(Long.parseLong(reportRequest.getReportId()));
                docUploadRequest.setType(documentType);
                docUploadRequest.setReportId(reportRequest.getReportId());
                try {
                    AddHouseBillToRepo(docUploadRequest, reportRequest.getPrintType(), pdfByteContent, tenantSettingsRow, shipmentDetails.getAdditionalDetails().getReleaseType(), StringUtility.convertToString(shipmentDetails.getGuid()));
                } catch (Exception e) {
                    log.error(e.getMessage());
                    //TODO - Abhimanyu doc upload failing
                    //throw new ValidationException("Unable to upload doc");
                }
            }

            if(reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Original.name()) || reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Surrender.name())){
                try
                {
                    //TODO - CMS Booking
//                    CmsBookingOrderRepository cms = new CmsBookingOrderRepository();
//                    if( (bool)tenantSettingsRow.CmsDoorOrder ){
//                        cms.sendCmsHBLUpdateFromFE(connection, shipmentsRow, tenantSettingsRow);
//                    }
                }
                catch(Exception e)
                {
                    // Todo add logger
                }
            }
        }
        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SEAWAY_BILL) && pdfByteContent != null)
        {
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(Long.parseLong(reportRequest.getReportId()));
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent())
                shipmentDetails = shipmentsRow.get();
            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(Constants.Shipments);
            docUploadRequest.setId(Long.parseLong(reportRequest.getReportId()));
            docUploadRequest.setType(ReportConstants.SEAWAY_BILL);
            docUploadRequest.setReportId(reportRequest.getReportId());
            try {
                AddHouseBillToRepo(docUploadRequest, TypeOfHblPrint.Draft.name().toUpperCase(), pdfByteContent, tenantSettingsRow, null, StringUtility.convertToString(shipmentDetails.getGuid()));
            } catch (Exception e) {
                log.error(e.getMessage());
                // TODO Abhimanyu doc upload failing
//                throw new ValidationException("Unable to upload doc");
            }
        }
        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SHIPPING_REQUEST )&& pdfByteContent != null)
        {
            String shipmentIds = dataRetrived.get(ReportConstants.SHIPMENT_IDS).toString();
            if (StringUtility.isNotEmpty(shipmentIds)) {
                List<String> shipmentIdList = Arrays.stream(shipmentIds.split(",")).toList();
                if (shipmentIdList != null && shipmentIdList.size() > 0) {
                    for(String shipmentId : shipmentIdList) {
                        createAutoEvent(shipmentId, EventConstants.SR_SENT_OR_NOT, tenantSettingsRow);
                    }
                }
            }
        }

        if (ObjectUtils.isNotEmpty(reportRequest.getReportInfo())) {
            String reportInfo = reportRequest.getReportInfo().toUpperCase();

            if (reportInfo.equals(ReportConstants.PICKUP_ORDER.toUpperCase())) {
                createAutoEvent(reportRequest.getReportId(), ReportConstants.PICKUP_ORDER_GEN, tenantSettingsRow);
            }

            if (reportInfo.equals(ReportConstants.SHIPMENT_CAN_DOCUMENT.toUpperCase())) {
                createEvent(reportRequest, EventConstants.CANG);
            }
        }
        if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.FCR_DOCUMENT)) {
            shipmentDao.updateFCRNo(Long.valueOf(reportRequest.getReportId()));
        }
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getPreAlertEmailAndLogs()) &&
                reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.PRE_ALERT)) {
            byte[] finalPdfByteContent = pdfByteContent;
            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(Constants.Shipments);
            docUploadRequest.setType(ReportConstants.PRE_ALERT);
            docUploadRequest.setIsTransferEnabled(true);
            Map<String, Object> finalDataRetrived = dataRetrived;
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addFilesFromReport(new BASE64DecodedMultipartFile(finalPdfByteContent), ReportConstants.PRE_ALERT + ".pdf", docUploadRequest, finalDataRetrived.get(GUID).toString())), executorService);
        }

        triggerAutomaticTransfer(report, reportRequest);
        return pdfByteContent;
    }

    private void createEvent(ReportRequest reportRequest, String eventCode) {
        if (reportRequest == null || reportRequest.getReportId() == null) {
            throw new IllegalArgumentException("Invalid report request or report ID.");
        }

        Long reportId;
        try {
            reportId = Long.parseLong(reportRequest.getReportId());
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("Invalid report ID format.", ex);
        }

        // Create EventsRequest
        EventsRequest eventsRequest = new EventsRequest();
        eventsRequest.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
        eventsRequest.setEntityId(reportId);
        eventsRequest.setEntityType(Constants.SHIPMENT);
        eventsRequest.setEventCode(eventCode);
        eventsRequest.setEventType(EventType.REPORT.name());
        eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getPreAlertEmailAndLogs()) && EventConstants.PRST.equals(eventCode)) {
            List<DocDetails> docDetails = docDetailsDao.findByEntityIdAndType(reportId, DocDetailsTypes.PRE_ALERT);
            String refNum;
            DocDetails docDetail;
            if(CommonUtils.listIsNullOrEmpty(docDetails)) {
                docDetail = DocDetails.builder()
                        .type(DocDetailsTypes.PRE_ALERT)
                        .entityId(reportId)
                        .build();
                refNum = "1";
            } else {
                docDetail = docDetails.get(0);
                refNum = String.valueOf(Integer.parseInt(docDetail.getVersionNumber()) + 1);
            }
            docDetail.setVersionNumber(refNum);
            docDetailsDao.save(docDetail);
            eventsRequest.setContainerNumber(refNum);
        }

        // Set reference number based on event code
        if (EventConstants.DHBL.equalsIgnoreCase(eventCode) || EventConstants.FHBL.equalsIgnoreCase(eventCode)) {
            shipmentDao.findById(reportId).ifPresent(shipmentDetails ->
                    eventsRequest.setContainerNumber(shipmentDetails.getHouseBill())
            );
        }

        // Save the event
        eventService.saveEvent(eventsRequest);
    }

    public void generatePdfBytes(ReportRequest reportRequest, DocPages pages, Map<String, Object> dataRetrived, List<byte[]> pdfBytes) {
        // Custom Air Labels
        if (Boolean.TRUE.equals(reportRequest.getPrintCustomLabel())) {
            if (reportRequest.isFromConsolidation() || reportRequest.getMawbNumber() != null) {
                // Master dialogue box
                dataRetrived.put(ReportConstants.AIRLINE_NAME, reportRequest.getConsolAirline());
                AWBLabelReport.populateMawb(dataRetrived, reportRequest.getMawbNumber());
                dataRetrived.put(ReportConstants.MAWB_CAPS, reportRequest.getMawbNumber());
                dataRetrived.put(ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE_CAPS, StringUtility.toUpperCase(reportRequest.getDestination()));
                dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, reportRequest.getTotalMawbPieces());
                if (reportRequest.getMawbNumber() != null) {
                    dataRetrived.put(ReportConstants.TOTAL_PACKS, reportRequest.getTotalMawbPieces());
                }
            } else {
                // House dialogue box
                dataRetrived.put(ReportConstants.HAWB_NUMBER, reportRequest.getHawbNumber());
                dataRetrived.put(ReportConstants.POD_AIRPORT_CODE_IN_CAPS, StringUtility.toUpperCase(reportRequest.getDestination()));
                dataRetrived.put(ReportConstants.TOTAL_PACKS, reportRequest.getTotalHawbPieces());
            }

            if (reportRequest.isCombiLabel()) {
                // Combi dialogue box
                List<Pair<String, Integer>> hawbPackageList = Optional.ofNullable(reportRequest.getHawbInfo())
                        .orElse(Collections.emptyList()).stream()
                        .filter(hawb -> hawb != null && (hawb.getHawbNumber() != null || hawb.getHawbPieceCount() != null))
                        .map(hawb -> Pair.of(
                                Objects.toString(hawb.getHawbNumber(), ""),
                                Objects.requireNonNullElse(hawb.getHawbPieceCount(), 0)
                        )).toList();

                dataRetrived.put(HAWB_PACKS_MAP, hawbPackageList);
                dataRetrived.put(ReportConstants.AIRLINE_NAME, reportRequest.getConsolAirline());
                AWBLabelReport.populateMawb(dataRetrived, reportRequest.getMawbNumber());
                dataRetrived.put(ReportConstants.MAWB_CAPS, reportRequest.getMawbNumber());
                dataRetrived.put(ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE_CAPS, StringUtility.toUpperCase(reportRequest.getDestination()));
                dataRetrived.put(ReportConstants.TOTAL_CONSOL_PACKS, reportRequest.getTotalMawbPieces());
            }
        }

        int copies = reportRequest.getCopyCountForAWB() != null ? reportRequest.getCopyCountForAWB() : 0;
        if(copies < 1) throw new ValidationException("Copy count is less than 1");
        Integer noOfPacks = 0;
        boolean isCombi = dataRetrived.containsKey(ReportConstants.IS_COMBI) && Boolean.TRUE.equals(dataRetrived.get(ReportConstants.IS_COMBI));
        List<Pair<String, Integer>> hawbPacksMap = null; // used only for combi label
        if(reportRequest.isFromConsolidation() && dataRetrived.get(ReportConstants.TOTAL_CONSOL_PACKS) != null) {
            noOfPacks = (Integer) dataRetrived.get(ReportConstants.TOTAL_CONSOL_PACKS);
        } else if (dataRetrived.get(ReportConstants.TOTAL_PACKS) != null) {
            noOfPacks = (Integer) dataRetrived.get(ReportConstants.TOTAL_PACKS);
        }
        if(isCombi) {
            hawbPacksMap = new ArrayList<>((List<Pair<String, Integer>>) dataRetrived.get(HAWB_PACKS_MAP));
            dataRetrived.remove(HAWB_PACKS_MAP);
            noOfPacks = (Integer) dataRetrived.get(ReportConstants.TOTAL_CONSOL_PACKS);
        }
        if(noOfPacks == null || noOfPacks == 0) {
            throw new ValidationException("no of pack is less than 1");
        }
        for(int i = 1; i <=copies; i++) {
            int ind = 0;
            int prevPacks = 0;
            for (int packs = 1; packs <= noOfPacks; packs++) {
                String packsCount = getSerialCount(packs, copies);
                String packsOfTotal = packs + "/" + noOfPacks;
                String hawbPacksCountForCombi = "";
                if(isCombi) {
                    dataRetrived.put(ReportConstants.HAWB_NUMBER, hawbPacksMap.get(ind).getKey());
                    packsOfTotal = (packs - prevPacks) + "/" + hawbPacksMap.get(ind).getValue();
                    hawbPacksCountForCombi = getSerialCount(packs - prevPacks, copies);
                    dataRetrived.put(COMBI_HAWB_COUNT, hawbPacksCountForCombi);
                    if((packs-prevPacks)%hawbPacksMap.get(ind).getValue() == 0) {
                        prevPacks = prevPacks + hawbPacksMap.get(ind).getValue();
                        ind++;
                    }
                }
                if (dataRetrived.get(ReportConstants.MAWB_NUMBER) != null || dataRetrived.get(ReportConstants.HAWB_NUMBER) != null) {
                    dataRetrived.put(ReportConstants.COUNT, packsCount);
                    dataRetrived.put(ReportConstants.PACKS_OF_TOTAL, packsOfTotal);
                    dataRetrived.put(ReportConstants.PACK_NUMBER, packs);
                }
                else dataRetrived.put(ReportConstants.COUNT, null);
                byte[] mainDocPage = GetFromDocumentService(dataRetrived, pages.getMainPageId());
                if (mainDocPage == null)
                    throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
                String mawbNumber = StringUtility.getEmptyString();
                String hawbNumber = StringUtility.getEmptyString();
                if(reportRequest.isFromConsolidation() || dataRetrived.get(ReportConstants.HAWB_NUMBER) == null ||
                        StringUtility.isEmpty(dataRetrived.get(ReportConstants.HAWB_NUMBER).toString()) || isCombi)
                    mawbNumber = dataRetrived.get(ReportConstants.MAWB_NUMBER) != null ? dataRetrived.get(ReportConstants.MAWB_NUMBER) + packsCount : packsCount;
                else
                    hawbNumber = dataRetrived.get(ReportConstants.HAWB_NUMBER) != null ? dataRetrived.get(ReportConstants.HAWB_NUMBER) + packsCount : packsCount;
                byte[] docBytes = addBarCodeInAWBLableReport(mainDocPage, mawbNumber, hawbNumber);
                if(isCombi) {
                    docBytes = addBarCodeForCombiReport(docBytes, dataRetrived.get(ReportConstants.HAWB_NUMBER) != null ? dataRetrived.get(ReportConstants.HAWB_NUMBER) + hawbPacksCountForCombi : hawbPacksCountForCombi);
                }
                pdfBytes.add(docBytes);
            }
        }
    }

    public byte[] GetFromDocumentService(Object json, String templateId) {
        try {
            if (Objects.isNull(templateId)) return null;
            DocumentRequest documentRequest = new DocumentRequest();
            documentRequest.setData(json);
            log.info("RequestId: {} | Event: {} | Template: {} | Request: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.DOCUMENT_SERVICE, templateId, jsonHelper.convertToJson(documentRequest));
            var response = documentService.downloadDocumentTemplate(jsonHelper.convertToJson(documentRequest), templateId);
            log.info("RequestId: {} | Event: {} | Template: {} | Response: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.DOCUMENT_SERVICE, templateId, jsonHelper.convertToJson(response));
            return response.getBody();
        } catch (Exception e) {
            log.error("RequestId: {} | Event: {} Error | Template: {} | Error: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.DOCUMENT_SERVICE, templateId, e.getMessage());
            log.error(e.getMessage());
            return null;
        }
    }


    public DocPages GetFromTenantSettings(String key, String hblType, String objectType, String printType, String frontTemplateCode, String  backTemplateCode,
                                          boolean isOriginalPrinted, String transportMode, String multiTemplateCode, boolean isTransportInstruction)
    {
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(1, UserContext.getUser().TenantId));
        if (shipmentSettingsDetailsList != null && shipmentSettingsDetailsList.size() >= 1)
        {
            ShipmentSettingsDetails admin = null;
            ShipmentSettingsDetails tenant = null;
            for(ShipmentSettingsDetails shipmentSettingsDetails : shipmentSettingsDetailsList) {
                if(shipmentSettingsDetails.getTenantId() == 1) {
                    admin = shipmentSettingsDetails;
                } else {
                    tenant = shipmentSettingsDetails;
                }
            }
            DocPages page = GetTemplateId(tenant, admin, key, hblType, objectType,
                    printType, frontTemplateCode, backTemplateCode, isOriginalPrinted, transportMode, multiTemplateCode, isTransportInstruction);
            if (page != null && Strings.isNullOrEmpty(page.getFirstPageId()) && Strings.isNullOrEmpty(page.getMainPageId()) && Strings.isNullOrEmpty(page.getBackPrintId())) {
                throw new ValidationException("Please upload template in branch settings for: " + key);
            }
            return page;

        }
        return null;
    }

    public DocPages GetTemplateId(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String DocKey, String HblType, String objectType, String printType,
                                         String frontTemplateCode, String backTemplateCode, boolean isOriginalPrinted, String transportMode, String multiTemplateCode, boolean istransportInstruction)
    {
        switch (DocKey)
        {
            case ReportConstants.SEAWAY_BILL:
                return setDocPages(null,
                        row.getSeawayMainPage() == null ? adminRow.getSeawayMainPage() : row.getSeawayMainPage(), null, row.getSeawayMainPage() != null, null, null,null);
            case ReportConstants.SHIP_TRUCKWAY_BILL:
                return setDocPages(null,
                        row.getShipTruckWayBillMainPage() == null ? adminRow.getShipTruckWayBillMainPage() : row.getShipTruckWayBillMainPage(), null, row.getShipTruckWayBillMainPage() != null, null, null, null);
            case ReportConstants.CONS_TRUCKWAY_BIll:
                return setDocPages(null,
                        row.getConsTruckWayBillMainPage() == null ? adminRow.getConsTruckWayBillMainPage() : row.getConsTruckWayBillMainPage(), null, row.getConsTruckWayBillMainPage() != null, null, null, null);
            case ReportConstants.SHIP_TRUCK_DRIVER_PROOF:
                return setDocPages(null,
                        row.getShipTruckDriverProof() == null ? adminRow.getShipTruckDriverProof() : row.getShipTruckDriverProof(), null, row.getShipTruckDriverProof() != null, null, null, null);
            case ReportConstants.CONS_TRUCK_DRIVER_PROOF:
                return setDocPages(null,
                        row.getConsTruckDriverProof() == null ? adminRow.getConsTruckDriverProof() : row.getConsTruckDriverProof(), null, row.getConsTruckDriverProof() != null, null, null, null);
            case ReportConstants.PACKING_LIST:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getPackingListMainPageAir() == null ? adminRow.getPackingListMainPageAir() : row.getPackingListMainPageAir(), null, row.getPackingListMainPageAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getPackingListMainPage() == null ? adminRow.getPackingListMainPage() : row.getPackingListMainPage(), null, row.getPackingListMainPage() != null, null, null, null);
                }
            case ReportConstants.CUSTOMS_INSTRUCTION:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getCustomsInsMainPageAir() == null ? adminRow.getCustomsInsMainPageAir() : row.getCustomsInsMainPageAir(), null, row.getCustomsInsMainPageAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getCustomsInsMainPage() == null ? adminRow.getCustomsInsMainPage() : row.getCustomsInsMainPage(), null, row.getCustomsInsMainPage() != null, null, null, null);
                }
            case ReportConstants.SHIPMENT_CAN_DOCUMENT:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getCanMainPageAir() == null ? adminRow.getCanMainPageAir() : row.getCanMainPageAir(), row.getCanBackPrintAir() == null ? adminRow.getCanBackPrintAir() : row.getCanMainPageAir(), row.getCanMainPageAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getCanMainPage() == null ? adminRow.getCanMainPage() : row.getCanMainPage(), row.getCanBackPrint() == null ? adminRow.getCanBackPrint() : row.getCanBackPrint(), row.getCanMainPage() != null, null, null, null);
                }
            case ReportConstants.AIRWAY_BILL:
                return setDocPages(null,
                        row.getAirwayMainPage() == null ? adminRow.getAirwayMainPage() : row.getAirwayMainPage(), null, row.getAirwayMainPage() != null, null, null, null);
            case ReportConstants.SHIPMENT_HOUSE_BILL:
                try
                {
                    if (frontTemplateCode != null || backTemplateCode != null)
                    {
                        String front = hblTermsConditionTemplateDao.getTemplateCode(frontTemplateCode, true ,printType).getTemplateFileName();
                        String back = null;
                        if (StringUtility.isNotEmpty(backTemplateCode))
                        {
                            back = hblTermsConditionTemplateDao.getTemplateCode(backTemplateCode, false ,printType).getTemplateFileName();
                        }
                        return setDocPages(null, front, back, true, null, null, row);
                    }
                }
                catch (ValidationException ex)
                {
                    return setDocPages(null,
                            row.getHouseMainPage() == null ? adminRow.getHouseMainPage() : row.getHouseMainPage(),
                            row.getHblFooter() == null ? adminRow.getHblFooter() : row.getHblFooter(), row.getHouseMainPage() != null, null, null, row);
                }
                return setDocPages(null,
                        row.getHouseMainPage() == null ? adminRow.getHouseMainPage() : row.getHouseMainPage(),
                        row.getHblFooter() == null ? adminRow.getHblFooter() : row.getHblFooter(), row.getHouseMainPage() != null, null, null, row);
            case ReportConstants.ARRIVAL_NOTICE:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getArrivalNoticeAir() == null ? adminRow.getArrivalNoticeAir() : row.getArrivalNoticeAir(), null, row.getArrivalNoticeAir() != null, null, null, null);
                } else {
                    return setDocPages(null,
                            row.getArrivalNotice() == null ? adminRow.getArrivalNotice() : row.getArrivalNotice(), null, row.getArrivalNotice() != null, null, null, null);
                }
            case ReportConstants.FREIGHT_CERTIFICATION:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getFreightCertificationAir() == null ? adminRow.getFreightCertificationAir() : row.getFreightCertificationAir(), null, true,null, null, null);
                }else{
                    return setDocPages(null,
                            row.getFreightCertification() == null ? adminRow.getFreightCertification() : row.getFreightCertification(), null, true, null, null, null);
                }
            case ReportConstants.PRE_ALERT:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getPreAlertAir() == null ? adminRow.getPreAlertAir() : row.getPreAlertAir(), null, row.getPreAlertAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getPreAlertDoc() == null ? adminRow.getPreAlertDoc() : row.getPreAlertDoc(), null, row.getPreAlertDoc() != null, null, null, null);
                }
            case ReportConstants.PROOF_OF_DELIVERY:
                return setDocPages(null,
                        row.getProofOfDelivery() == null ? adminRow.getProofOfDelivery() : row.getProofOfDelivery(), null, row.getProofOfDelivery() != null, null, null, null);
            case ReportConstants.PICKUP_ORDER:
                if(istransportInstruction){
                    return setDocPages(null,
                            row.getTransportInstructionPickupOrder() == null ? adminRow.getTransportInstructionPickupOrder() : row.getTransportInstructionPickupOrder(), null, row.getTransportInstructionPickupOrder() != null, null, null, null);
                }
                else if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getPickupOrderAir() == null ? adminRow.getPickupOrderAir() : row.getPickupOrderAir(), null, row.getPickupOrderAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getPickupOrder() == null ? adminRow.getPickupOrder() : row.getPickupOrder(), null, row.getPickupOrder() != null, null ,null, null);
                }
            case ReportConstants.DELIVERY_ORDER:
                if(istransportInstruction){
                    return setDocPages(null,
                            row.getTransportInstructionDeliveryOrder() == null ? adminRow.getTransportInstructionDeliveryOrder() : row.getTransportInstructionDeliveryOrder(), null, row.getTransportInstructionDeliveryOrder() != null, null, null, null);
                }
                else if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getDeliveryOrderAir() == null ? adminRow.getDeliveryOrderAir() : row.getDeliveryOrderAir(), null, row.getDeliveryOrderAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getDeliveryOrder() == null ? adminRow.getDeliveryOrder() : row.getDeliveryOrder(), null, row.getDeliveryOrder() != null, null, null, null);
                }
            case ReportConstants.BOOKING_CONFIRMATION:
                if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
                    return setDocPages(null,
                            row.getBookingConfirmationAir() == null ? adminRow.getBookingConfirmationAir() : row.getBookingConfirmationAir(), null, row.getBookingConfirmationAir() != null, null, null, null);
                }else{
                    return setDocPages(null,
                            row.getBookingConfirmation() == null ? adminRow.getBookingConfirmation() : row.getBookingConfirmation(), null, row.getBookingConfirmation() != null, null, null, null);
                }
            case ReportConstants.COSTAL_DOC:
                return setDocPages(null,
                        row.getCostalDocument() == null ? adminRow.getCostalDocument() : row.getCostalDocument(), null, row.getCostalDocument() != null, null, null, null);
            case ReportConstants.SHIPPING_INSTRUCTION:
                return getShippingInstructionDocument(row, adminRow, objectType);
            case ReportConstants.AWB_LABEL:
                return setDocPages(null,
                        row.getAwbLable() == null ? adminRow.getAwbLable() : row.getAwbLable(), null, row.getAwbLable() != null, null, null, null);
            case ReportConstants.CARGO_MANIFEST:
                return setDocPages(null,
                        row.getCargoManifest() == null ? adminRow.getCargoManifest() : row.getCargoManifest(), null, row.getCargoManifest() != null, null, null, null);
            case ReportConstants.CONSOLIDATED_PACKING_LIST:
                return setDocPages(null,
                        row.getConsolidatedPackingList() == null ? adminRow.getConsolidatedPackingList() : row.getConsolidatedPackingList(), null, row.getConsolidatedPackingList() != null, null, null, null);
            case ReportConstants.HAWB:
                return setDocPages(null,
                        row.getHawb() == null ? adminRow.getHawb() : row.getHawb(), null, row.getHawb() != null, null, null, null);
            case ReportConstants.MAWB:
                return setDocPages(null,
                        row.getMawb() == null ? adminRow.getMawb() : row.getMawb(), null, row.getMawb() != null, null, null, null);
            case ReportConstants.AWB_NEUTRAL:
                return setDocPages(null,
                        row.getAwbNeutral() == null ? adminRow.getAwbNeutral() : row.getAwbNeutral(), null, row.getAwbNeutral() != null, null, null, null);
            case ReportConstants.SHIPPING_REQUEST:
                return setDocPages(null,
                        row.getShippingRequestMainPage() == null ? adminRow.getShippingRequestMainPage(): row.getShippingRequestMainPage(), null, row.getShippingRequestMainPage() != null, null, null, null);
            case ReportConstants.SHIPPING_REQUEST_AIR:
                return setDocPages(null,
                        row.getShippingRequestAir() == null ? adminRow.getShippingRequestAir(): row.getShippingRequestAir(), null, row.getShippingRequestAir() != null, null, null, null);
            case ReportConstants.IMPORT_SHIPMENT_MANIFEST:
                if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
                    return setDocPages(null,
                            row.getAirImportShipmentManifest() == null ? adminRow.getAirImportShipmentManifest(): row.getAirImportShipmentManifest(), null, row.getAirImportShipmentManifest() != null, null, null, null);
                } else {
                    return setDocPages(null,
                            row.getSeaImportShipmentManifest() == null ? adminRow.getSeaImportShipmentManifest(): row.getSeaImportShipmentManifest(), null, row.getSeaImportShipmentManifest() != null, null, null, null);
                }
            case ReportConstants.EXPORT_SHIPMENT_MANIFEST:
                if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
                    return setDocPages(null,
                            row.getAirExportShipmentManifest() == null ? adminRow.getAirExportShipmentManifest(): row.getAirExportShipmentManifest(), null, row.getAirExportShipmentManifest() != null, null, null, null);
                } else {
                    return setDocPages(null,
                            row.getSeaExportShipmentManifest() == null ? adminRow.getSeaExportShipmentManifest(): row.getSeaExportShipmentManifest(), null, row.getSeaExportShipmentManifest() != null, null, null, null);
                }
            case ReportConstants.CARGO_MANIFEST_AIR_IMPORT_SHIPMENT:
                return setDocPages(null,
                        row.getAirImportShipmentManifest() == null ? adminRow.getAirImportShipmentManifest(): row.getAirImportShipmentManifest(), null, row.getAirImportShipmentManifest() != null, null, null, null);
            case ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION:
                return setDocPages(null,
                        row.getAirImportConsoleManifest() == null ? adminRow.getAirImportConsoleManifest(): row.getAirImportConsoleManifest(), null, row.getAirImportConsoleManifest() != null, null, null, null);
            case ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT:
                return setDocPages(null,
                        row.getAirExportShipmentManifest() == null ? adminRow.getAirExportShipmentManifest(): row.getAirExportShipmentManifest(), null, row.getAirExportShipmentManifest() != null, null, null, null);
            case ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION:
                return setDocPages(null,
                        row.getAirExportConsoleManifest() == null ? adminRow.getAirExportConsoleManifest(): row.getAirExportConsoleManifest(), null, row.getAirExportConsoleManifest() != null, null, null, null);
            case ReportConstants.IMPORT_CONSOL_MANIFEST:
                if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
                    return setDocPages(null,
                            row.getAirImportConsoleManifest() == null ? adminRow.getAirImportConsoleManifest(): row.getAirImportConsoleManifest(), null, row.getAirImportConsoleManifest() != null, null, null, null);
                } else {
                    return setDocPages(null,
                            row.getSeaImportConsoleManifest() == null ? adminRow.getSeaImportConsoleManifest(): row.getSeaImportConsoleManifest(), null, row.getSeaImportConsoleManifest() != null, null, null, null);
                }
            case ReportConstants.EXPORT_CONSOL_MANIFEST:
                if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
                    return setDocPages(null,
                            row.getAirExportConsoleManifest() == null ? adminRow.getAirExportConsoleManifest() : row.getAirExportConsoleManifest(), null, row.getAirExportConsoleManifest() != null, null, null, null);
                } else {
                    return setDocPages(null,
                            row.getSeaExportConsoleManifest() == null ? adminRow.getSeaExportConsoleManifest() : row.getSeaExportConsoleManifest(), null, row.getSeaExportConsoleManifest() != null, null, null, null);
                }
            case ReportConstants.CSR:
                return setDocPages(null,
                        row.getCsr() == null ? adminRow.getCsr() : row.getCsr(), null, row.getCsr() != null, null, null, null);

            case ReportConstants.COMMERCIAL_INVOICE:
                if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR))
                    return setDocPages(null,
                            row.getCommercialInvMainPageAir() == null ? adminRow.getCommercialInvMainPageAir() : row.getCommercialInvMainPageAir(), null, row.getCommercialInvMainPageAir() != null, null, null, null);
                else
                    return setDocPages(null,
                            row.getCommercialInvMainPage() == null ? adminRow.getCommercialInvMainPage() : row.getCommercialInvMainPage(), null, row.getCommercialInvMainPage() != null, null, null, null);
            case ReportConstants.GENERATE_ISF_FILE:
                return setDocPages(null,
                        row.getIsfFileMainPage() == null ? adminRow.getIsfFileMainPage() : row.getIsfFileMainPage(), null, row.getIsfFileMainPage() != null, null, null, null);
            case ReportConstants.CONTAINER_MANIFEST_PRINT:
                return setDocPages(null,
                        row.getContainerManifestPrint() == null ? adminRow.getContainerManifestPrint(): row.getContainerManifestPrint(), null, row.getContainerManifestPrint() != null, null, null, null);
            case ReportConstants.MANIFEST_PRINT:
                return setDocPages(null,
                        row.getManifestPrint() == null ? adminRow.getManifestPrint(): row.getManifestPrint(), null, row.getManifestPrint() != null, null, null, null);
            case ReportConstants.TRANSPORT_ORDER:
                return setDocPages(null,
                        row.getTransportOrderRoad() == null ? adminRow.getTransportOrderRoad(): row.getTransportOrderRoad(), null, row.getTransportOrderRoad() != null, null, null, null);
            case ReportConstants.BOOKING_ORDER:
                if (transportMode.equalsIgnoreCase(ReportConstants.AIR)){
//                    if(objectType.equalsIgnoreCase(Constants.DMAWB))
//                        return setDocPages(null,
//                            row.getBookingOrderAirForMawb() == null ? adminRow.getBookingOrderAirForMawb() : row.getBookingOrderAirForMawb(), null, row.getBookingOrderAirForMawb() != null, null, null, null);
                    return setDocPages(null,
                        row.getBookingOrderAir() == null ? adminRow.getBookingOrderAir() : row.getBookingOrderAir(), null, row.getBookingOrderAir() != null, null, null, null);
                }else{
//                    if(objectType.equalsIgnoreCase(Constants.DMAWB)) // using key : DMAWB for sea also
//                        return setDocPages(null,
//                            row.getBookingOrderForMbl() == null ? adminRow.getBookingOrderForMbl() : row.getBookingOrderForMbl(), null, row.getBookingOrderForMbl() != null, null, null, null);
                    return setDocPages(null,
                        row.getBookingOrder() == null ? adminRow.getBookingOrder() : row.getBookingOrder(), null, row.getBookingOrder() != null, null, null, null);
                }
            case ReportConstants.CSD_REPORT:
                return setDocPages(null,
                        row.getCsd() == null ? adminRow.getCsd() : row.getCsd(), null, row.getCsd() != null, null, null, null);
           case ReportConstants.FCR_DOCUMENT:
                return setDocPages(null,
                        row.getFcrDocument() == null ? adminRow.getFcrDocument() : row.getFcrDocument(), null, row.getFcrDocument() != null, null, null, null);
            default:
        }

        return null;
    }

    public static DocPages setDocPages(String FirstPageId, String MainPageId, String LastPageId, boolean isLogoFixed,
                                       String multiTemplateCode, String entityType, ShipmentSettingsDetails tenantRow)
    {
        DocPages docPages = new DocPages();
        if (StringUtility.isNotEmpty(multiTemplateCode) && StringUtility.isNotEmpty(entityType)) {
            //docPages.MainPageId = getMultiTemplateCode(multiTemplateCode, entityType);
        } else {
            docPages.setMainPageId(MainPageId);
        }

        docPages.setFirstPageId(FirstPageId);
        docPages.setBackPrintId(LastPageId);
        if (docPages.getMainPageId() != null) {
            isLogoFixed = true;
        }
        docPages.setLogoFixed(isLogoFixed);
        docPages.setShipmentSettingsDetails(tenantRow);

        return docPages;
    }

    public DocPages getShippingInstructionDocument(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType)
    {
//        if (row.getRep != null && row.ReportingNewFlow.Value)
//        {
//            return setDocPages(null,
//                    row.SeaShippingInstructionMainPage == null ? adminRow.SeaShippingInstructionMainPage :
//                            row.SeaShippingInstructionMainPage, null, row.SeaShippingInstructionMainPage == null ? false : true);
//        }
        if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null,
                    row.getShippingInstruction() == null ? adminRow.getShippingInstruction() : row.getShippingInstruction(), null, row.getShippingInstruction() != null, null, null, null);
        }
        else {
            return setDocPages(null,
                    row.getSeaShippingInstructionMainPage() == null ? adminRow.getSeaShippingInstructionMainPage() : row.getSeaShippingInstructionMainPage(), null, row.getSeaShippingInstructionMainPage() != null, null, null, null);
        }
    }

    public String getSerialCount(int copyNumber, int totalCopies){
        String _copy_count = Integer.toString(copyNumber);
        String _total_copies = Integer.toString(totalCopies);
        String ans = _copy_count;
        int size = _copy_count.length();
        if(_copy_count.length() < 5 && _total_copies.length() < 5){
            for(int i=0; i<5-size; i++){
                ans = "0" + ans;
            }
        }else if(_copy_count.length() < 5){
            int total_copies_size = _total_copies.length();
            for(int i=0; i<total_copies_size-size; i++){
                ans = "0" + ans;
            }
        }
        return ans;
    }

    public byte[] addBarCodeForCombiReport(byte[] bytes, String hawbNumber) {
        if(StringUtility.isNotEmpty(hawbNumber))
            bytes = this.addBarCodeInReport(bytes, hawbNumber, 10, -225, ReportConstants.HAWB, true);
        return bytes;
    }

    public byte[] addBarCodeInAWBLableReport(byte[] bytes, String mawbNumber, String hawbNumber)
    {
        if(StringUtility.isNotEmpty(mawbNumber) && mawbNumber.length() > 5)
            bytes = this.addBarCodeInReport(bytes, mawbNumber,10,-75, ReportConstants.MAWB, true);
        else if(StringUtility.isNotEmpty(hawbNumber))
            bytes = this.addBarCodeInReport(bytes, hawbNumber, 10, -75, ReportConstants.HAWB, true);
        return bytes;
    }

    private byte[] addBarCodeInReport(byte[] bytes, String str, int x, int y, String docType, boolean isAirlabel) throws ValidationException {
        if (StringUtility.isEmpty(str)) return bytes;
        if (CommonUtils.HasUnsupportedCharacters(str)) {
            if (docType != null) {
                throw new ValidationException(docType + " number consists of unsupported characters, Please check and re-generate.");
            } else {
                throw new ValidationException("Unsupported characters, Please check and re-generate.");
            }
        }

        ByteArrayOutputStream ms = new ByteArrayOutputStream(MAX_BUFFER_SIZE);
        PdfReader reader = null;

        try {
            reader = new PdfReader(bytes);
            PdfStamper stamper = new PdfStamper(reader, ms);
            Rectangle realPageSize = reader.getPageSizeWithRotation(1);
            PdfContentByte dc = stamper.getOverContent(1);
            PdfGState gstate = new PdfGState();
            dc.saveState();
            dc.setGState(gstate);

            // Generate barcode image
            byte[] imgBytes1 = CommonUtils.generateBarcodeImage(str);
            Image image1 = Image.getInstance(imgBytes1);
            if(isAirlabel) {
                image1.scaleAbsolute(250, 30);
            } else {
                image1.scaleAbsolute(300, 30);
            }
            image1.setAbsolutePosition((int) realPageSize.getLeft() + x, realPageSize.getTop() + y);
            dc.addImage(image1);

            dc.restoreState();

            stamper.close();
            reader.close();
            byte[] data = ms.toByteArray();
            ms.reset();
            return data;
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return null;
    }

    private byte[] getBytesForNeutralAWB(Object json){
        DocPages Pages = GetFromTenantSettings(ReportConstants.AWB_NEUTRAL, null, null, null, null, null, false, null, null,false);;
        return GetFromDocumentService(json, Pages.getMainPageId());
    }

    public List<byte[]> getOriginalandCopies(DocPages pages, String ReportInfo, byte[] mainDoc, byte[] firstpage, byte[] backprint, final Map<String, Object> json, String hbltype, ShipmentSettingsDetails shipmentSettings, String noOfCopies, ReportRequest reportRequest) throws DocumentException, IOException {

        List<byte[]> pdfBytes = new ArrayList<>();

        String logopath = (String)json.getOrDefault(ReportConstants.LOGO, null);


        if (pages.isLogoFixed() || isHblType(hbltype, ReportInfo))
        {
            logopath = null;
        }

        int originalCount = Integer.parseInt((String) json.getOrDefault(ReportConstants.ORIGINALS, -1).toString());
        int copyCount = Integer.parseInt((String) json.getOrDefault(ReportConstants.COPY_BILLS, -1).toString());

        if (!ReportInfo.equalsIgnoreCase(ReportConstants.SHIPMENT_HOUSE_BILL))
        {
            originalCount = -1;
            copyCount = -1;

            byte[] pdfByteContentOriginal = mergeDocumentBytes(mainDoc, firstpage, backprint, logopath, ReportInfo, pages.getShipmentSettingsDetails());
            pdfBytes.add(pdfByteContentOriginal);
        }
        else if ( originalCount != 0 )
        {
            // use a concurrent dictionary instead of a list for thread safety and preserving the order of document
            Map<Integer, byte[]> mainDocParallel = new ConcurrentHashMap<>();
            // Call document service in parallel
            IntStream.range(1, originalCount + 1).parallel().forEach(i -> {
                        // make a deep clone of jsonDict to avoid race cases
                        Map<String, Object> jsonDictClone = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(json));
                        jsonDictClone.put(ReportConstants.INCREMENTAL_ORIGINALS, i);
                        byte[] mainDocCurrent;
                        if(reportRequest.isPrintForParties()){
                            try {
                                mainDocCurrent = printForPartiesAndBarcode(reportRequest, new ArrayList<>(), json.get(ReportConstants.HAWB_NO) == null? "" : json.get(ReportConstants.HAWB_NO).toString(), jsonDictClone, pages);
                            } catch (DocumentException | IOException e) {
                                throw new RuntimeException(e);
                            }
                        }else{
                            mainDocCurrent = GetFromDocumentService(jsonDictClone, pages.getMainPageId());
                        }
                        mainDocParallel.put(i, mainDocCurrent);
                    }
            );

            for(int i = 1; i <= originalCount; i++){
                byte[] pdfByteContentCurrent = mergeDocumentBytes(mainDocParallel.get(i), firstpage, backprint, logopath, ReportInfo, pages.getShipmentSettingsDetails());
                pdfBytes.add(pdfByteContentCurrent);
            }

        }

        if (copyCount > 0)
        {
            json.put(ReportConstants.ORIGINAL_OR_COPY, ReportConstants.COPY);
            json.put(ReportConstants.CHARGES, json.get(ReportConstants.COPY_CHARGES));
            json.put(ReportConstants.AS_AGREED, json.get(ReportConstants.COPY_AS_AGREED));
            if(reportRequest.isPrintForParties()){
                mainDoc = printForPartiesAndBarcode(reportRequest, new ArrayList<>(), json.get(ReportConstants.HAWB_NO) == null? "" : json.get(ReportConstants.HAWB_NO).toString(), json, pages);
            }else{
                mainDoc = GetFromDocumentService(json, pages.getMainPageId());
            }
            byte[] pdfByteContentCopy = mergeDocumentBytes(mainDoc, firstpage, backprint, logopath, ReportInfo, pages.getShipmentSettingsDetails());
            for (int i = 0; i < copyCount; i++)
            {
                pdfBytes.add(pdfByteContentCopy);
            }

        }

        try {
            if (!Objects.isNull(shipmentSettings) && !Objects.isNull(shipmentSettings.getRestrictBlRelease())
                    && shipmentSettings.getRestrictBlRelease() && StringUtility.isNotEmpty(noOfCopies)) {
                Integer _copy = Integer.parseInt(noOfCopies);
                byte[] pdfByteContentCopy = mergeDocumentBytes(mainDoc, firstpage, backprint, logopath, ReportInfo, pages.getShipmentSettingsDetails());
                while (_copy-- > 1) {
                    pdfBytes.add(pdfByteContentCopy);
                }
            }
        } catch (Exception ex) { /* Ignore */ }
        return pdfBytes;

    }

    public byte[] mergeDocumentBytes(byte[] mainDoc, byte[] firstPage, byte[] backPrint, String logoPath, String reportInfo, ShipmentSettingsDetails tenantRow) throws DocumentException, IOException {
        ByteArrayOutputStream destinationDocumentStream = new ByteArrayOutputStream();

        PdfConcatenate pdfConcat = new PdfConcatenate(destinationDocumentStream);
        PdfReader pdfReader1 = null;
        List<Integer> pages = new ArrayList<>();
        pages.add(1);

        if (firstPage != null) {
            pdfReader1 = new PdfReader(firstPage);
            pdfReader1.selectPages(pages);
            pdfConcat.addPages(pdfReader1);
        }

        if (reportInfo.equalsIgnoreCase(ReportConstants.SHIPMENT_HOUSE_BILL) && tenantRow.getPrintAfterEachPage()) {
            PdfReader pdfReader = new PdfReader(mainDoc);
            int totalPages = pdfReader.getNumberOfPages();
            for (int i = 1; i <= totalPages; i++) {
                PdfReader pdfReader_maindoc = new PdfReader(mainDoc);
                pages = new ArrayList<>();
                pages.add(i);
                pdfReader_maindoc.selectPages(pages);
                pdfConcat.addPages(pdfReader_maindoc);

                if (backPrint != null) {
                    PdfReader pdfReader_backdoc = new PdfReader(backPrint);
                    List<Integer> pages2 = new ArrayList<>();
                    pages2.add(1);
                    pdfReader_backdoc.selectPages(pages2);
                    pdfConcat.addPages(pdfReader_backdoc);
                }
                pdfReader_maindoc.close();
            }
            pdfReader.close();
        } else {
            pdfReader1 = new PdfReader(mainDoc);
            pages = new ArrayList<>();
            for (int i = 1; i <= pdfReader1.getNumberOfPages(); i++) {
                pages.add(i);
            }
            pdfReader1.selectPages(pages);
            pdfConcat.addPages(pdfReader1);

            if (backPrint != null) {
                pages = new ArrayList<>();
                pages.add(1);
                pdfReader1 = new PdfReader(backPrint);
                pdfReader1.selectPages(pages);
                pdfConcat.addPages(pdfReader1);
            }
        }

        if(pdfReader1 != null) {
            pdfReader1.close();
        }
        pdfConcat.close();
        byte[] data = destinationDocumentStream.toByteArray();
        destinationDocumentStream.reset();
        return addImage(data, logoPath);
    }

    private Boolean isHblType(String type, String key)
    {
        return key.equalsIgnoreCase(ReportConstants.SHIPMENT_HOUSE_BILL) && type != null;
    }

    public byte[] addImage(byte[] inputBytes, String logopath) throws IOException, DocumentException {

        if (StringUtility.isEmpty(logopath))
            return inputBytes;
//        logopath = GetBaseUrl() + "/" + logopath;
//        //TODO- don't exact requirement
////        System.Net.ServicePointManager.ServerCertificateValidationCallback += (sender, certificate, chain, errors) =>
////        {
////            return true;
////        };
//        // Stream inputImageStream =  new MemoryStream(imageData);
//        OutputStream outputPdfStream = new ByteArrayOutputStream();
//        PdfReader reader = new PdfReader(inputBytes);
//        PdfStamper stamper = new PdfStamper(reader, outputPdfStream);
//
//        PdfWriter writer = stamper.getWriter();
//
//        for (int i = 1; i <= reader.getNumberOfPages(); i++)
//        {
//            PdfDictionary pg = reader.getPageN(i);
//            PdfDictionary res = (PdfDictionary)PdfReader.getPdfObject(pg.get(PdfName.RESOURCES));
//            PdfDictionary xobj = (PdfDictionary)PdfReader.getPdfObject(res.get(PdfName.XOBJECT));
//            if (xobj == null) continue;
//            for(PdfName name : xobj.getKeys())
//            {
//                PdfObject obj = xobj.get(name);
//                if (obj.isIndirect())
//                {
//                    Image image = Image.getInstance(logopath);
//                    PdfDictionary tg = (PdfDictionary)PdfReader.getPdfObject(obj);
//                    PdfName type = tg.getAsName(PdfName.SUBTYPE);
//                    String width = tg.get(PdfName.WIDTH).toString();
//                    String height = tg.get(PdfName.HEIGHT).toString();
//                    if (PdfName.IMAGE.equals(type))
//                    {
//                        PdfReader.killIndirect(obj);
//                        Image maskImage = image.getImageMask();
//                        if (maskImage != null)
//                            writer.addDirectImageSimple(maskImage);
//                        writer.addDirectImageSimple(image, (PRIndirectReference)obj);
//                    }
//
//                }
//            }
//
//        }
//        stamper.close();
//        reader.close();
//        return ((ByteArrayOutputStream)outputPdfStream).toByteArray();
        return inputBytes;
    }

    public String GetBaseUrl()
    {
        return null;
//        var httpContext = Serenity.Dependency.TryResolve<IHttpContextAccessor>().HttpContext;
//        var request = httpContext.Request;
//        var host = request.Host.ToUriComponent();
//        var pathBase = request.PathBase.ToUriComponent();
//        return $"{request.Scheme}://{host}{pathBase}";
    }

    private void createAutoEvent(String ReportId, String eventCode, ShipmentSettingsDetails tenantSettingsRow) {
        if (tenantSettingsRow.getAutoEventCreate() && StringUtility.isNotEmpty(ReportId)) {
            CustomAutoEventRequest eventReq = new CustomAutoEventRequest();
            eventReq.entityId = Long.parseLong(ReportId);
            eventReq.entityType = Constants.SHIPMENT;
            eventReq.eventCode = eventCode;
            eventDao.autoGenerateEvents(eventReq);
        }
    }

    public void AddHouseBillToRepo(DocUploadRequest uploadRequest, String printType, byte[] document, ShipmentSettingsDetails shipmentSettingsDetails, String releaseType, String shipmentGuid) throws IOException {
        List<Hbl> blObjectList = hblDao.findByShipmentId(Long.parseLong(uploadRequest.getReportId()));
        if (blObjectList == null || blObjectList.isEmpty())
            return;
        Hbl blObject = blObjectList.get(0);
        String fileVersion = null;
        if (printType.equalsIgnoreCase(TypeOfHblPrint.Original.name()) && blObject != null && blObject.getHblData() != null) {
            fileVersion = blObject.getHblData().getOriginalSeq() != null ? StringUtility.convertToString(blObject.getHblData().getOriginalSeq()) : null;
            blObject.getHblData().setOriginalSeq(blObject.getHblData().getOriginalSeq() != null ? blObject.getHblData().getOriginalSeq() + 1 : 1);
            updateInReleaseMappingTable(blObject, releaseType, shipmentSettingsDetails);
            uploadRequest.setIsTransferEnabled(Boolean.TRUE);
            hblDao.save(blObject);
        } else {
            fileVersion = blObject.getHblData().getVersion().toString();
            blObject.getHblData().setVersion(blObject.getHblData().getVersion() + 1);
            hblDao.save(blObject);
        }
        String filename = uploadRequest.getType() + "_" + printType + "_" + uploadRequest.getId() + "_" + fileVersion + ".pdf";

        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addFilesFromReport(new BASE64DecodedMultipartFile(document), filename, uploadRequest, shipmentGuid)), executorService);

    }

    private void updateInReleaseMappingTable(Hbl hbl, String releaseType, ShipmentSettingsDetails shipmentSettings) {
        if (StringUtility.isNotEmpty(releaseType) && !Objects.isNull(shipmentSettings) && !Objects.isNull(shipmentSettings.getRestrictBlRelease())
                && shipmentSettings.getRestrictBlRelease()) {
            List<HblReleaseTypeMapping> releaseTypeMappingList = hblReleaseTypeMappingDao.findByReleaseTypeAndHblId(hbl.getId(), releaseType);
            HblReleaseTypeMapping releaseTypeMapping;
            if (releaseTypeMappingList == null || releaseTypeMappingList.isEmpty()) {
                // create new
                releaseTypeMapping = HblReleaseTypeMapping.builder()
                        .hblId(hbl.getId())
                        .releaseType(releaseType)
                        .copiesPrinted(1)
                        .build();
            }
            else {
                releaseTypeMapping = releaseTypeMappingList.get(0);
                releaseTypeMapping.setCopiesPrinted(releaseTypeMapping.getCopiesPrinted() + 1);
            }
            hblReleaseTypeMappingDao.save(releaseTypeMapping);
        }
    }

    public void addDocumentToDocumentMaster(ReportRequest reportRequest,  byte[] pdfByte_Content) {
        try {
            boolean isShipment = reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB);
            String guid = null;
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(Long.parseLong(reportRequest.getReportId()));
            ShipmentDetails shipmentDetails;
            if(shipmentsRow.isPresent()) {
                shipmentDetails = shipmentsRow.get();
                isShipment = true; // DRT shipment , printing MAWB from shipment
                guid = StringUtility.convertToString(shipmentDetails.getGuid());
            }

            if(!isShipment){
                Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(Long.parseLong(reportRequest.getReportId()));
                ConsolidationDetails consolidationDetails;
                if(optionalConsolidationDetails.isPresent()){
                    consolidationDetails = optionalConsolidationDetails.get();
                    guid = StringUtility.convertToString(consolidationDetails.getGuid());
                }
            }

            if(guid == null) {
                throw new RunnerException("Report Id is Invalid");
            }

            byte[] finalPdfByte_Content = pdfByte_Content;
            String documentType = documentTypeFinder(reportRequest);

            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(isShipment ? Constants.Shipments : Constants.Consolidations);
            docUploadRequest.setId(Long.parseLong(reportRequest.getReportId()));
            docUploadRequest.setType(documentType);
            docUploadRequest.setReportId(reportRequest.getReportId());
            if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL))
                docUploadRequest.setIsTransferEnabled(Boolean.TRUE);
            String filename = docUploadRequest.getType() + "_" + reportRequest.getPrintType() + "_" + docUploadRequest.getId() + ".pdf";
            String finalGuid = guid;
            CompletableFuture.runAsync(masterDataUtils.withMdc(
                () -> addFilesFromReport(new BASE64DecodedMultipartFile(finalPdfByte_Content), filename,
                    docUploadRequest, finalGuid)), executorService);


            if(reportRequest.isPrintCSD() && ReportConstants.ORIGINAL.equalsIgnoreCase(reportRequest.getPrintType())){
                addCSDDocumentToDocumentMaster(reportRequest.getReportId(), docUploadRequest, guid);
            }
        }catch(Exception ex){
            log.error(ex.getMessage());
        }
    }

    public void addCSDDocumentToDocumentMaster(String reportId, DocUploadRequest docUploadRequest, String guid)
        throws DocumentException, RunnerException, IOException, ExecutionException, InterruptedException {
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId(reportId);
        reportRequest.setReportInfo(CSD_REPORT);
        if(Constants.Consolidations.equalsIgnoreCase(docUploadRequest.getEntityType())){
            reportRequest.setFromConsolidation(true);
        }
        try{
        CommonRequestModel commonRequestModel =  CommonRequestModel.buildRequest(reportRequest);
        DocUploadRequest csdDocumentUploadRequest = new DocUploadRequest(docUploadRequest);
        csdDocumentUploadRequest.setType(CSD_REPORT);
        String filename = CSD_REPORT + "_" + docUploadRequest.getId() + ".pdf";

        byte[] pdfByte_Content = getDocumentData(commonRequestModel);
      CompletableFuture.runAsync(masterDataUtils.withMdc(
            () -> addFilesFromReport(new BASE64DecodedMultipartFile(pdfByte_Content), filename,
                csdDocumentUploadRequest, guid)), executorService);
            MDC.put(Constants.IS_CSD_DOCUMENT_ADDED, "true");
      } catch (Exception e) {
            MDC.put(Constants.IS_CSD_DOCUMENT_ADDED, "false");
            log.error(e.getMessage());
        }
    }

    private String documentTypeFinder(ReportRequest reportRequest){
        String documentType = ReportConstants.HAWB;

        if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) {
            if (reportRequest.getPrintType().equalsIgnoreCase("ORIGINAL")) {
                documentType = ReportConstants.ORIGINAL_HAWB;
            } else if (reportRequest.getPrintType().equalsIgnoreCase("DRAFT")) {
                documentType = ReportConstants.DRAFT_HAWB;
            }
        }else if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB)){
            documentType = ReportConstants.MAWB;
            if (reportRequest.getPrintType().equalsIgnoreCase("ORIGINAL")) {
                documentType = ReportConstants.ORIGINAL_MAWB;
            } else if (reportRequest.getPrintType().equalsIgnoreCase("DRAFT")) {
                documentType = ReportConstants.DRAFT_MAWB;
            }
        }

        return documentType;
    }

    public DocumentManagerResponse<DocumentManagerDataResponse> addFilesFromReport(MultipartFile file, String filename, DocUploadRequest uploadRequest, String entityKey) {
        try {
            var uploadResponse = documentManagerService.temporaryFileUpload(file, filename);
            if (!uploadResponse.getSuccess())
                throw new IOException("File Upload Failed");

            var saveResponse = documentManagerService.saveFile(DocumentManagerSaveFileRequest.builder().fileName(filename)
                    .entityType(uploadRequest.getEntityType())
                    .secureDownloadLink(uploadResponse.getData().getSecureDownloadLink())
                    .fileSize(uploadResponse.getData().getFileSize())
                    .fileType(uploadResponse.getData().getFileType())
                    .path(uploadResponse.getData().getPath())
                    .entityKey(entityKey)
                    .source(Constants.SYSTEM_GENERATED)
                    .docType(uploadRequest.getType())
                    .docName(uploadRequest.getType())
                    .childType(uploadRequest.getType())
                    .isTransferEnabled(uploadRequest.getIsTransferEnabled())
                    .build());
            return saveResponse;
        } catch (Exception ex) {
            log.error("Error while file upload : {}", ex.getLocalizedMessage());
        }
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> createDocumentTagsForShipment(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request =  (CommonGetRequest)commonRequestModel.getData();
        if(request.getId() == null && request.getGuid() == null) {
            log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id and GUID can't be null. Please provide any one !");
        }
        Long id = request.getId();
        Optional<ShipmentDetails> shipmentDetails = Optional.ofNullable(null);
        if(request.getId() != null ){
            shipmentDetails = shipmentDao.findById(id);
        } else {
            UUID guid = UUID.fromString(request.getGuid());
            shipmentDetails = shipmentDao.findByGuid(guid);
        }
        if (!shipmentDetails.isPresent()) {
            log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Map<String, Object> dataRetrived = new HashMap<>();
        shipmentTagsForExteranlServices.populateRaKcDataWithShipmentDetails(dataRetrived, shipmentDetails.get());
        return ResponseHelper.buildSuccessResponse(dataRetrived);
    }

    private void setPrintTypeForAwb(ReportRequest reportRequest, Boolean isOriginalPrint) {
        var originalPrintedAt = LocalDateTime.now();
        if((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) && Boolean.TRUE.equals(isOriginalPrint)) {
            if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && !reportRequest.isFromShipment())
                awbDao.updateAwbPrintInformation(null, Long.parseLong(reportRequest.getReportId()), PrintType.ORIGINAL_PRINTED, isOriginalPrint, originalPrintedAt);
            else
                awbDao.updateAwbPrintInformation(Long.parseLong(reportRequest.getReportId()), null, PrintType.ORIGINAL_PRINTED, isOriginalPrint, originalPrintedAt);
        } else if ((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) && reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)) {
            if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && !reportRequest.isFromShipment())
                awbDao.updateAwbPrintInformation(null, Long.parseLong(reportRequest.getReportId()), PrintType.DRAFT_PRINTED, isOriginalPrint, null);
            else
                awbDao.updateAwbPrintInformation(Long.parseLong(reportRequest.getReportId()), null, PrintType.DRAFT_PRINTED, isOriginalPrint, null);
        }
    }

    private LocalDateTime getCurrentTimeInTenantTimeZone() {
        var timeZone = UserContext.getUser().getTimeZoneId();
        if(timeZone == null)
            return LocalDateTime.now();
        return LocalDateTime.now(ZoneId.of(timeZone));
    }

    private byte[] printForPartiesAndBarcode(ReportRequest reportRequest, List<byte[]> pdf_Bytes, String number, Map<String, Object> dataRetrived, DocPages pages) throws DocumentException, IOException {
        String[] printingForParties = null;
        byte[] lastPage = null;
        if(reportRequest.getPrintingFor_str() != null && reportRequest.getPrintingFor_str().equalsIgnoreCase("0")){
            printingForParties = new String[]{"1","2","3","4","5","6","7","8","9","10","11","12"};
        } else if(StringUtility.isNotEmpty(reportRequest.getPrintingFor_str())){
            printingForParties = reportRequest.getPrintingFor_str().split(",");
        }
        for(String party : printingForParties){
            MawbPrintFor printForParty = MawbPrintFor.getById(Integer.parseInt(party));
            dataRetrived.put(ReportConstants.PRINTING_FOR , printForParty.getDesc());
            byte[] mainDocPage = GetFromDocumentService(dataRetrived, pages.getMainPageId());
            if(mainDocPage == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
            else{
                if(lastPage == null) lastPage = CommonUtils.getLastPage(mainDocPage);
                if (Boolean.FALSE.equals(printForParty.getPrintTermsAndCondition())) {
                    mainDocPage = CommonUtils.removeLastPage(mainDocPage);
                    mainDocPage = CommonUtils.addBlankPage(mainDocPage);
                }
            }
            if(reportRequest.isPrintBarcode())
                mainDocPage = addBarCodeInReport(mainDocPage, number, 140, -50, ReportConstants.MAWB, false);
            pdf_Bytes.add(mainDocPage);
        }
        return CommonUtils.concatAndAddContent(pdf_Bytes);
    }

    public void triggerAutomaticTransfer(IReport report, ReportRequest reportRequest){
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if (!Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled())) {
                return;
            }
            if (reportRequest.getPrintType()!=null && !reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL)) {
                return;
            }
            Long reportId = Long.parseLong(reportRequest.getReportId());

            if (report instanceof HblReport) {
                ShipmentDetails shipmentDetails = getShipmentDetails(reportRequest);
                if(shipmentDetails!=null) {
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerBlAutomaticTransfer(shipmentDetails)), executorService);
                }
            } else if (report instanceof HawbReport) {
                ShipmentDetails shipmentDetails = getShipmentDetails(reportRequest);
                if(shipmentDetails!=null) {
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerHAWBAutomaticTransfer(shipmentDetails)), executorService);
                }
            } else if (report instanceof MawbReport) {
                if (!reportRequest.isFromShipment()) { // Case: Request came from consolidation
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerConsoleMAWBAutomaticTransfer(reportId)), executorService);
                } else {
                    ShipmentDetails shipmentDetails = getShipmentDetails(reportRequest);
                    if(shipmentDetails!=null) {
                        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerShipmentMAWBAutomaticTransfer(shipmentDetails)), executorService);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error while triggering automatic transfer for report {}, errorMsg: {}", reportRequest.getReportInfo(), e.getMessage());
        }
    }

    private ShipmentDetails getShipmentDetails(ReportRequest reportRequest){
        Long shipmentId = Long.parseLong(reportRequest.getReportId());
        long startTimeForShipment = System.currentTimeMillis();
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).orElse(null);
        if(shipmentDetails!=null) {
            if(shipmentDetails.getConsolidationList()!=null)
                log.info(String.valueOf(shipmentDetails.getConsolidationList().size()));
            log.info(ReportConstants.TIME_TAKE_TO_GET_SHIPMENT_CONSOLE_DATA, shipmentId, System.currentTimeMillis() - startTimeForShipment);
        }
        return shipmentDetails;
    }

    public void triggerBlAutomaticTransfer(ShipmentDetails shipmentDetails){
        if(!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())){
            for(ConsolidationDetails consolidationDetails: shipmentDetails.getConsolidationList()){
                if (consolidationDetails!=null  &&
                        (Objects.equals(Constants.TRANSPORT_MODE_SEA, consolidationDetails.getTransportMode()) &&
                                !Objects.equals(Constants.CONSOLIDATION_TYPE_DRT, consolidationDetails.getConsolidationType())))
                    consolidationService.triggerAutomaticTransfer(consolidationDetails, null, true);
            }
        }
    }

    public void triggerHAWBAutomaticTransfer(ShipmentDetails shipmentDetails){
        if(ObjectUtils.isNotEmpty(shipmentDetails.getConsolidationList())){
            for(ConsolidationDetails consolidationDetails: shipmentDetails.getConsolidationList()){
                if (consolidationDetails!=null &&
                        (Objects.equals(Constants.TRANSPORT_MODE_AIR, consolidationDetails.getTransportMode()) &&
                                Objects.equals(Constants.SHIPMENT_TYPE_STD, shipmentDetails.getJobType())))
                    consolidationService.triggerAutomaticTransfer(consolidationDetails, null, true);
            }
        }
    }

    public void triggerConsoleMAWBAutomaticTransfer(Long consolidationId){
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consolidationId).orElse(null);
        if(consolidationDetails==null)
            return;
        if(Objects.equals(Constants.TRANSPORT_MODE_AIR, consolidationDetails.getTransportMode()) &&
                Objects.equals(Constants.SHIPMENT_TYPE_STD, consolidationDetails.getConsolidationType())) {
            consolidationService.triggerAutomaticTransfer(consolidationDetails, null, true);
        }
    }

    public void triggerShipmentMAWBAutomaticTransfer(ShipmentDetails shipmentDetails){
        if(Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()) &&
                Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType())) {
            shipmentService.triggerAutomaticTransfer(shipmentDetails, null, true);
        }
    }

    public EmailBodyResponse getPreAlertEmailTemplateData(Long shipmentId, Long emailTemplateId) throws RunnerException {
        EmailBodyResponse response = new EmailBodyResponse();
        Map<String, Object> map = new HashMap<>();
        List<EmailTemplatesRequest> emailTemplatesRequests = new ArrayList<>();
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId)
                .orElseThrow(() -> new DataRetrievalFailureException("No Shipment found with Id: " + shipmentId));
        populateTagsAndEmailTemplate(shipmentDetails, map, emailTemplateId, emailTemplatesRequests, toEmailIds, ccEmailIds);
        if(CommonUtils.listIsNullOrEmpty(emailTemplatesRequests))
            throw new RunnerException("No Template Found!");

        response.setSubject(commonUtils.replaceTagsFromData(map, emailTemplatesRequests.get(0).getSubject()));
        response.setBody(commonUtils.replaceTagsFromData(map, emailTemplatesRequests.get(0).getBody()));
        response.setTo(toEmailIds.isEmpty() ? null : String.join(",", toEmailIds));
        response.setCc(ccEmailIds.isEmpty() ? null : String.join(",", ccEmailIds));
        response.setTags(List.of(
                TagsData.builder()
                        .tagName(SHIPMENT_PRE_ALERT_DOC)
                        .tagValue(shipmentDetails.getGuid().toString())
                        .build()));
        return response;
    }

    public void getEmailTemplate(Long emailTemplateId, List<EmailTemplatesRequest> emailTemplatesRequests) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List <Object> criteria1 = Arrays.asList(List.of(EntityTransferConstants.ID), "=", emailTemplateId);
        List<Object> criteria2 = new ArrayList<>(List.of(List.of(TENANTID), "=", TenantContext.getCurrentTenant()));
        request.setCriteriaRequests(List.of(criteria1, "and", criteria2));
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        emailTemplatesRequests.addAll(jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class));
    }

    public void populateTagsAndEmailTemplate(ShipmentDetails shipmentDetails, Map<String, Object> map, Long emailTemplateId, List<EmailTemplatesRequest> emailTemplatesRequests, Set<String> to, Set<String> cc) {
        try {to.add(shipmentDetails.getAdditionalDetails().getImportBroker().getOrgData().get("Email").toString());} catch (Exception ignored) {log.error("Email not available for DA for Pre Alert Email");}
        map.put(CBN_NUMBER, shipmentDetails.getBookingReference());
        map.put(MODE, shipmentDetails.getTransportMode());
        map.put(LOAD, shipmentDetails.getShipmentType());
        map.put(SHIPPER, getPartyString(shipmentDetails.getConsigner()));
        map.put(CNEES, getPartyString(shipmentDetails.getConsignee()));
        map.put(ETD_CAPS, shipmentDetails.getCarrierDetails().getEtd());
        map.put(ETA_CAPS, shipmentDetails.getCarrierDetails().getEta());
        map.put(HOUSE_BILL, shipmentDetails.getHouseBill());
        map.put(MASTER_BILL, shipmentDetails.getMasterBill());
        map.put(CONT_NO, getContNums(shipmentDetails));
        map.put(CARRIER, shipmentDetails.getCarrierDetails().getShippingLine());
        map.put(CBR, shipmentDetails.getBookingNumber());
        map.put(COMMODITY, shipmentDetails.getCommodity());
        map.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        try {
            map.put(OA_BRANCH, shipmentDetails.getAdditionalDetails().getExportBroker().getOrgData().get(FULL_NAME));
            map.put(OA_BRANCH_ADD, String.join(", ", IReport.getPartyAddress(modelMapper.map(shipmentDetails.getAdditionalDetails().getExportBroker(), PartiesModel.class))));
            map.put(OA_NAME, shipmentDetails.getAdditionalDetails().getExportBroker().getAddressData().get(CONTACT_PERSON));
            map.put(OA_PHONE, shipmentDetails.getAdditionalDetails().getExportBroker().getOrgData().get(PHONE));
            map.put(OA_EMAIL, shipmentDetails.getAdditionalDetails().getExportBroker().getOrgData().get(EMAIL));
        } catch (Exception e) {log.error("Error while getting origin Agent Data");}
        try {
            map.put(DA_BRANCH, shipmentDetails.getAdditionalDetails().getImportBroker().getOrgData().get(FULL_NAME));
            map.put(DA_BRANCH_ADD, String.join(", ", IReport.getPartyAddress(modelMapper.map(shipmentDetails.getAdditionalDetails().getImportBroker(), PartiesModel.class))));
            map.put(DA_NAME, shipmentDetails.getAdditionalDetails().getImportBroker().getAddressData().get(CONTACT_PERSON));
            map.put(DA_PHONE, shipmentDetails.getAdditionalDetails().getImportBroker().getOrgData().get(PHONE));
            map.put(DA_EMAIL, shipmentDetails.getAdditionalDetails().getImportBroker().getOrgData().get(EMAIL));
        } catch (Exception e) {log.error("Error while getting destination Agent Data");}
        Map<String, EntityTransferUnLocations> unLocMap = new HashMap<>();
        Set<String> usernamesList = new HashSet<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getCreatedBy()))
            usernamesList.add(shipmentDetails.getCreatedBy());
        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            usernamesList.add(shipmentDetails.getAssignedTo());

        var unlocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(),
                shipmentDetails.getCarrierDetails().getDestinationPort(),
                shipmentDetails.getCarrierDetails().getOrigin(),
                shipmentDetails.getCarrierDetails().getDestination()).filter(Objects::nonNull).collect(Collectors.toSet()), unLocMap)));
        var templatesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getEmailTemplate(emailTemplateId, emailTemplatesRequests)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);

        CompletableFuture.allOf(unlocationsFuture, templatesFuture, userEmailsFuture).join();

        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOrigin()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOrigin()))
            map.put(ORIGIN, unLocMap.get(shipmentDetails.getCarrierDetails().getOrigin()).getName());
        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestination()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestination()))
            map.put(DSTN, unLocMap.get(shipmentDetails.getCarrierDetails().getDestination()).getName());
        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort()))
            map.put(POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort()))
            map.put(POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());

        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
            cc.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
        if(!CommonUtils.IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
            cc.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
    }

    private String getPartyString(Parties parties) {
        if(Objects.isNull(parties))
            return null;
        return String.join(", ", ReportHelper.getOrgAddress(modelMapper.map(parties, PartiesModel.class)));
    }

    private String getContNums(ShipmentDetails shipmentDetails) {
        List<String> response = new ArrayList<>();
        if(!CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            shipmentDetails.getContainersList().stream().filter(e -> !CommonUtils.IsStringNullOrEmpty(e.getContainerNumber())).forEach(e -> response.add(e.getContainerNumber()));
        }
        return String.join(", ", response);
    }

}
