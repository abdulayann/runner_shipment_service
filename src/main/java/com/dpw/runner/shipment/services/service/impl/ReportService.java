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
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
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
import com.dpw.runner.shipment.services.utils.NetworkTransferV3Util;
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
import org.springframework.beans.factory.annotation.Qualifier;
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
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
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

    @Qualifier("executorServiceReport")
    @Autowired
    ExecutorService executorServiceReport;
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
    private NetworkTransferV3Util networkTransferV3Util;

    @Autowired
    private IV1Service iv1Service;

    @Autowired
    private IDocDetailsDao docDetailsDao;

    @Autowired
    @Lazy
    private ShipmentTagsForExteranlServices shipmentTagsForExteranlServices;

    private static final int MAX_BUFFER_SIZE = 10 * 1024;
    private static final String INVALID_REPORT_KEY = "This document is not yet configured, kindly reach out to the support team";
    @Autowired
    private ReportService self;

    @Override
    @Transactional
    public byte[] getDocumentData(CommonRequestModel request)
        throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException {
        ReportRequest reportRequest = (ReportRequest) request.getData();

        // Generate combined shipment report via consolidation
        byte[] dataForCombinedReport = getDataForCombinedReport(reportRequest);
        if (dataForCombinedReport != null) return dataForCombinedReport;

        // CargoManifestAirExportConsolidation , validate original awb printed for its HAWB
        validateOriginalAwbPrintForLinkedShipment(reportRequest);

        byte[] dataByteList = getCombinedDataForCargoManifestAir(reportRequest);
        if (dataByteList != null) {
            pushFileToDocumentMaster(reportRequest, dataByteList, new HashMap<>());
            return dataByteList;
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
        validateForInvalidReport(report);

        validateDpsForMawbReport(report, reportRequest, isOriginalPrint);

        Map<String, Object> dataRetrived;

        if(report instanceof AWBLabelReport awbLabelReport1) {
            awbLabelReport1.setMawb(reportRequest.isFromConsolidation());
            awbLabelReport1.setRemarks(reportRequest.getRemarks());
            awbLabelReport1.setCombi(reportRequest.isCombiLabel());
            awbLabelReport1.setCustomLabel(reportRequest.getPrintCustomLabel() != null && reportRequest.getPrintCustomLabel());
        }
        if(report instanceof FCRDocumentReport fcrDocumentReport) {
            fcrDocumentReport.setFcrShipper(reportRequest.getFcrShipper());
            fcrDocumentReport.setPackIds(reportRequest.getPackIds());
            fcrDocumentReport.setIssueDate(reportRequest.getDateOfIssue());
            fcrDocumentReport.setPlaceOfIssue(reportRequest.getPlaceOfIssue());
        }
        // user story 135668
        setPrintWithoutTranslation(report, reportRequest);
        updateCustomDataCargoManifestAirReport(report, reportRequest);

        if(report instanceof CSDReport csdReport) {
            csdReport.setIsConsolidation(reportRequest.isFromConsolidation());
        }

        //LATER - Need to handle for new flow
        dataRetrived = getDocumentDataForReports(report, reportRequest);

        String hbltype = (String)dataRetrived.getOrDefault(ReportConstants.HOUSE_BILL_TYPE, null);
        String objectType = getObjectType(reportRequest, dataRetrived);

        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.AWB_LABEL)){
            List<byte[]> pdfBytes = new ArrayList<>();
            DocPages pages = getFromTenantSettings(reportRequest.getReportInfo(), null, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), null, false);
            generatePdfBytes(reportRequest, pages, dataRetrived, pdfBytes);
            var byteContent = CommonUtils.concatAndAddContent(pdfBytes);
            pushFileToDocumentMaster(reportRequest, byteContent, dataRetrived);
            return byteContent;
        }
        else if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB))
        {
            return getBytesForMawb(reportRequest, dataRetrived, isOriginalPrint, isSurrenderPrint, report);
        }
        else if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB))
        {
            return getBytesForHawb(reportRequest, dataRetrived, isOriginalPrint, isSurrenderPrint, isNeutralPrint, hbltype, objectType, tenantSettingsRow, report);
        }
        else if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.BOOKING_ORDER)) {
            return getBytesForBookingOrderReport(dataRetrived, reportRequest);
        }

        updateDocumentPrintType(reportRequest, dataRetrived);

        if (dataRetrived.containsKey(ReportConstants.TRANSPORT_MODE)){
            objectType = dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString();
        }

        DocPages pages = getFromTenantSettings(reportRequest.getReportInfo(), objectType, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), reportRequest.getTransportMode(), StringUtility.isNotEmpty(reportRequest.getTransportInstructionId()));
        if (pages == null)
        {
            return null;
        }
        Map<String, Object> retrived = dataRetrived;
        var mainDocFuture = CompletableFuture.supplyAsync(() -> getFromDocumentService(retrived, pages.getMainPageId()), executorService);
        var firstPageFuture = CompletableFuture.supplyAsync(() -> getFromDocumentService(retrived, pages.getFirstPageId()), executorService);
        var backPrintFuture = CompletableFuture.supplyAsync(() -> getFromDocumentService(retrived, pages.getBackPrintId()), executorService);
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
        boolean waterMarkRequired = getWaterMarkRequired(reportRequest);

        pdfByteContent = CommonUtils.concatAndAddContent(pdfBytes);
        BaseFont font = BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED);

        pdfByteContent = getPdfBytesForHouseBill(reportRequest, dataRetrived, waterMarkRequired, pdfByteContent, font, isOriginalPrint, isSurrenderPrint, isNeutralPrint, tenantSettingsRow);
        addHBLToRepoForSeawayBill(reportRequest, pdfByteContent, tenantSettingsRow);
        createEventsForReportInfo(reportRequest, pdfByteContent, dataRetrived, tenantSettingsRow);
        if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.FCR_DOCUMENT)) {
            shipmentDao.updateFCRNo(Long.valueOf(reportRequest.getReportId()));
        }

        processPreAlert(reportRequest, pdfByteContent, dataRetrived);

        triggerAutomaticTransfer(report, reportRequest);
        // Push document to document master
        pushFileToDocumentMaster(reportRequest, pdfByteContent, dataRetrived);
        return pdfByteContent;
    }

    private void validateForInvalidReport(IReport report) {
        if (Objects.isNull(report))
            throw new ValidationException(INVALID_REPORT_KEY);
    }

    private void processPreAlert(ReportRequest reportRequest, byte[] pdfByteContent, Map<String, Object> dataRetrived) {
        if(Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getPreAlertEmailAndLogs()) &&
                reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.PRE_ALERT)) {
            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS);
            docUploadRequest.setDocType(ReportConstants.PRE_ALERT);
            docUploadRequest.setIsTransferEnabled(true);
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addFilesFromReport(new BASE64DecodedMultipartFile(pdfByteContent), ReportConstants.PRE_ALERT + ".pdf", docUploadRequest, dataRetrived.get(GUID).toString())), executorService);
        }
    }

    private void updateCustomDataCargoManifestAirReport(IReport report, ReportRequest reportRequest) {
        if(report instanceof CargoManifestAirConsolidationReport cargoManifestAirConsolidationReport) {
            cargoManifestAirConsolidationReport.setShipIds(reportRequest.getShipmentIds());
            cargoManifestAirConsolidationReport.setShipperAndConsignee(reportRequest.isShipperAndConsignee());
            cargoManifestAirConsolidationReport.setSecurityData(reportRequest.isSecurityData());
        }
        if(report instanceof CargoManifestAirShipmentReport cargoManifestAirShipmentReport) {
            cargoManifestAirShipmentReport.setShipperAndConsignee(reportRequest.isShipperAndConsignee());
            cargoManifestAirShipmentReport.setSecurityData(reportRequest.isSecurityData());
        }
    }

    private void setPrintWithoutTranslation(IReport report, ReportRequest reportRequest) {
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
    }

    private void processPushAwbEventForMawb(ReportRequest reportRequest, Boolean isOriginalPrint) {
        Optional<Awb> awb = Optional.ofNullable(this.setPrintTypeForAwb(reportRequest, isOriginalPrint));

        if(Boolean.TRUE.equals(reportRequest.getPushAwbEvent()) && reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && Boolean.TRUE.equals(isOriginalPrint)) {
            awbDao.airMessagingIntegration(Long.parseLong(reportRequest.getReportId()), reportRequest.getReportInfo(), reportRequest.isFromShipment(), reportRequest.isIncludeCsdInfo());
        } else if((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) && Boolean.TRUE.equals(isOriginalPrint)) {
            if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && !reportRequest.isFromShipment())
                awbDao.updateAirMessageStatusFromConsolidationId(Long.parseLong(reportRequest.getReportId()), AwbStatus.AWB_ORIGINAL_PRINTED.name());
            else
                awbDao.updateAirMessageStatusFromShipmentId(Long.parseLong(reportRequest.getReportId()), AwbStatus.AWB_ORIGINAL_PRINTED.name());

            awb.ifPresent(value -> value.setAirMessageStatus(AwbStatus.AWB_ORIGINAL_PRINTED));
        }
    }

    private String getObjectType(ReportRequest reportRequest, Map<String, Object> dataRetrived) {
        String objectType = "";
        if (ReportConstants.OBJECT_TYPE_REPORTS.contains(reportRequest.getReportInfo()) && dataRetrived.containsKey(ReportConstants.OBJECT_TYPE))
        {
            objectType = dataRetrived.get(ReportConstants.OBJECT_TYPE).toString();
        }
        return objectType;
    }

    private void updateDocumentPrintType(ReportRequest reportRequest, Map<String, Object> dataRetrived) {
        if (StringUtility.isNotEmpty(reportRequest.getPrintType()))
        {
            String documentPrintType = "ZERO (0)"; //Draft
            if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL))
            {
                documentPrintType = "THREE (3)"; //Original
            }
            else if(reportRequest.getPrintType().equalsIgnoreCase("SURRENDER"))
            {
                documentPrintType = "ONE (1)"; //Surrender
            }

           dataRetrived.put(ReportConstants.DOCUMENT_PRINT_TYPE, documentPrintType);
        }
    }

    private byte[] getBytesForBookingOrderReport(Map<String, Object> dataRetrived, ReportRequest reportRequest) {
        String consolidationType = dataRetrived.get(ReportConstants.SHIPMENT_TYPE) != null ?
            dataRetrived.get(ReportConstants.SHIPMENT_TYPE).toString() : null;
        String transportMode = ReportConstants.SEA;

        if (dataRetrived.containsKey(ReportConstants.TRANSPORT_MODE)){
            transportMode = dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString();
        }

        DocPages pages = getFromTenantSettings(reportRequest.getReportInfo(), consolidationType, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), transportMode, false);

        byte[] pdfByteContent = getFromDocumentService(dataRetrived, pages.getMainPageId());
        if(pdfByteContent == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);

        // Push document to document master
        pushFileToDocumentMaster(reportRequest, pdfByteContent, dataRetrived);

        return pdfByteContent;
    }

    private byte[] getBytesForHawb(ReportRequest reportRequest, Map<String, Object> dataRetrived, Boolean isOriginalPrint, Boolean isSurrenderPrint, Boolean isNeutralPrint, String hbltype, String objectType, ShipmentSettingsDetails tenantSettingsRow, IReport report) throws RunnerException, DocumentException, IOException, InterruptedException, ExecutionException {
        updateCustomDataInDataRetrivedForHawb(reportRequest, dataRetrived);

        updateDateAndStatusForHawbPrint(reportRequest, dataRetrived, isOriginalPrint, isSurrenderPrint, isNeutralPrint);

        List<byte[]> pdfBytes = new ArrayList<>();
        if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.NEUTRAL))
            return getBytesForNeutralAWB(dataRetrived);

        DocPages docPages = getFromTenantSettings(reportRequest.getReportInfo(), objectType, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), reportRequest.getTransportMode(),false);
        byte[] pdfByteContent;
        byte[] mainDocHawb = null;
        CompletableFuture<byte[]> mainDocFuture = null;
        boolean asyncFlag = Boolean.FALSE;
        if(reportRequest.isPrintForParties()){
            mainDocHawb = printForPartiesAndBarcode(reportRequest, pdfBytes, dataRetrived.get(ReportConstants.HAWB_NO) == null? "" : dataRetrived.get(ReportConstants.HAWB_NO).toString(), dataRetrived, docPages);
        }else{
            asyncFlag = Boolean.TRUE;
            mainDocFuture = CompletableFuture.supplyAsync(
                () -> getFromDocumentService(dataRetrived, docPages.getMainPageId()),
                executorService);
        }
        var firstPageHawbFuture =  CompletableFuture.supplyAsync(
             () -> getFromDocumentService(dataRetrived, docPages.getFirstPageId()),
             executorService);
        var backPageHawbFuture =  CompletableFuture.supplyAsync(
            () -> getFromDocumentService(dataRetrived, docPages.getBackPrintId()),
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
        List<byte[]> pdfBytesHawb = getOriginalandCopies(docPages, reportRequest.getReportInfo(), mainDocHawb, firstPageHawb, backPrintHawb, dataRetrived, hbltype, tenantSettingsRow, reportRequest.getNoOfCopies(), reportRequest);
        pdfByteContent = getPdfByteContentForHawb(reportRequest, dataRetrived, isOriginalPrint, pdfBytesHawb);

        addDocumentToDocumentMaster(reportRequest, pdfByteContent);

        processPushAwbEventForMawb(reportRequest, isOriginalPrint);
        triggerAutomaticTransfer(report, reportRequest);

        // Push document to document master
        pushFileToDocumentMaster(reportRequest, pdfByteContent, dataRetrived);

        //Update shipment issue date
        return pdfByteContent;
    }

    private void updateCustomDataInDataRetrivedForHawb(ReportRequest reportRequest, Map<String, Object> dataRetrived) {
        if (!reportRequest.isPrintIATAChargeCode()) {
            dataRetrived.remove(ReportConstants.OTHER_CHARGES_IATA);
        }
        if(!reportRequest.isPrintCSD()){
            dataRetrived.remove(RA_CSD);
        }
        if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount())
        {
            dataRetrived.put(ReportConstants.PACKING_LIST, dataRetrived.get(ReportConstants.PACKING_LIST_FAT));
            dataRetrived.put(ReportConstants.SUM_OF_TOTAL_AMOUNT, Constants.EMPTY_STRING);
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
        if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && Boolean.TRUE.equals(reportRequest.getDisplayOtherAmount()))
        {
            dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_OTHERS_P));
            dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_OTHERS_C));
        }
        if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && !Boolean.TRUE.equals(reportRequest.getDisplayOtherAmount()))
        {
            dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_P));
            dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_C));
        }
    }

    private byte[] getPdfByteContentForHawb(ReportRequest reportRequest, Map<String, Object> dataRetrived, Boolean isOriginalPrint, List<byte[]> pdfBytesHawb) throws DocumentException, IOException {
        byte[] pdfByteContent;
        pdfByteContent = CommonUtils.concatAndAddContent(pdfBytesHawb);
        if (pdfByteContent == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
        var shc = dataRetrived.getOrDefault(ReportConstants.SPECIAL_HANDLING_CODE, null);
        boolean addWaterMarkForEaw = false;
        if(shc != null){
            List<String> items = CommonUtils.splitAndTrimStrings(shc.toString());
            if(!items.isEmpty() && items.contains(Constants.EAW)){
                addWaterMarkForEaw = true;
            }
        }
        if(addWaterMarkForEaw && reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Draft.name())) {
            pdfByteContent = CommonUtils.addWatermarkToPdfBytes(pdfByteContent, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_EAW_WATERMARK);
        }
        else if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Draft.name()))
        {
            pdfByteContent = CommonUtils.addWatermarkToPdfBytes(pdfByteContent, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_WATERMARK);
        } else if(addWaterMarkForEaw && Boolean.TRUE.equals(isOriginalPrint)) {
            pdfByteContent = CommonUtils.addWatermarkToPdfBytes(pdfByteContent, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.ORIGINAL_EAW_WATERMARK);
        }
        return pdfByteContent;
    }

    private void updateDateAndStatusForHawbPrint(ReportRequest reportRequest, Map<String, Object> dataRetrived, Boolean isOriginalPrint, Boolean isSurrenderPrint, Boolean isNeutralPrint) throws RunnerException {
        if (isOriginalPrint || isSurrenderPrint || Boolean.TRUE.equals(isNeutralPrint))
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
            Integer statusVal = status != null ? status.getValue() : null;
            shipmentService.updateDateAndStatus(Long.parseLong(reportRequest.getReportId()), issueDate, statusVal);
        }
    }

    private byte[] getBytesForMawb(ReportRequest reportRequest, Map<String, Object> dataRetrived, Boolean isOriginalPrint, Boolean isSurrenderPrint, IReport report) throws DocumentException, IOException, RunnerException {
        updateCustomDataInDataRetrivedForMawb(reportRequest, dataRetrived);
        List<byte[]> pdfBytes = new ArrayList<>();
        if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.NEUTRAL)) {
            return getBytesForNeutralAWB(dataRetrived);
        }
        byte[] pdfByteContentForMawb = getPdfByteContentForMawb(reportRequest, dataRetrived, pdfBytes);
        var shc = dataRetrived.getOrDefault(ReportConstants.SPECIAL_HANDLING_CODE, null);
        boolean addWaterMarkForEaw = false;
        if(shc != null){
            List<String> items = CommonUtils.splitAndTrimStrings(shc.toString());
            if(!items.isEmpty() && items.contains(Constants.EAW)){
                addWaterMarkForEaw = true;
            }
        }
        if(addWaterMarkForEaw && reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Draft.name())) {
            pdfByteContentForMawb = CommonUtils.addWatermarkToPdfBytes(pdfByteContentForMawb, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_EAW_WATERMARK);
        }
        else if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)){
            pdfByteContentForMawb = CommonUtils.addWatermarkToPdfBytes(pdfByteContentForMawb, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.DRAFT_WATERMARK);
        } else if(addWaterMarkForEaw && Boolean.TRUE.equals(isOriginalPrint)) {
            pdfByteContentForMawb = CommonUtils.addWatermarkToPdfBytes(pdfByteContentForMawb, BaseFont.createFont(BaseFont.TIMES_BOLD, BaseFont.WINANSI, BaseFont.EMBEDDED), ReportConstants.ORIGINAL_EAW_WATERMARK);
        }
        //Update shipment issue date
        if ((isOriginalPrint || isSurrenderPrint) && reportRequest.getReportKey() != null && reportRequest.getReportKey().equalsIgnoreCase(ReportConstants.SHIPMENT_ID))
        {
            shipmentService.updateDateAndStatus(Long.parseLong(reportRequest.getReportId()), LocalDate.now().atStartOfDay(), null);
        }

        addDocumentToDocumentMaster(reportRequest, pdfByteContentForMawb);
        processPushAwbEventForMawb(reportRequest, isOriginalPrint);

        triggerAutomaticTransfer(report, reportRequest);
        pushFileToDocumentMaster(reportRequest, pdfByteContentForMawb, dataRetrived);
        return pdfByteContentForMawb;
    }

    private byte[] getPdfByteContentForMawb(ReportRequest reportRequest, Map<String, Object> dataRetrived, List<byte[]> pdfBytes) throws DocumentException, IOException {
        DocPages docPages = getFromTenantSettings(reportRequest.getReportInfo(), null, reportRequest.getPrintType(), reportRequest.getFrontTemplateCode(), reportRequest.getBackTemplateCode(), null, false);
        byte[] pdfByteContent = null;
        if(reportRequest.isPrintForParties()){
            pdfByteContent = printForPartiesAndBarcode(reportRequest, pdfBytes, dataRetrived.get(ReportConstants.MAWB_NUMBER) == null ? "": dataRetrived.get(ReportConstants.MAWB_NUMBER).toString(), dataRetrived, docPages);
        }else{
            pdfByteContent = getFromDocumentService(dataRetrived, docPages.getMainPageId());
            if(pdfByteContent == null) throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
        }
        return pdfByteContent;
    }

    private void updateCustomDataInDataRetrivedForMawb(ReportRequest reportRequest, Map<String, Object> dataRetrived) {
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
            dataRetrived.put(ReportConstants.SUM_OF_TOTAL_AMOUNT, Constants.EMPTY_STRING);
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
        if(reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && Boolean.TRUE.equals(reportRequest.getDisplayOtherAmount()))
        {
            dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.TOTAL_OTHERS_P));
            dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.TOTAL_OTHERS_C));
        }
        if (reportRequest.getDisplayFreightAmount() != null && !reportRequest.getDisplayFreightAmount() && reportRequest.getDisplayOtherAmount() != null && Boolean.TRUE.equals(!reportRequest.getDisplayOtherAmount()))
        {
            dataRetrived.put(ReportConstants.TOTAL_PREPAID, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_P));
            dataRetrived.put(ReportConstants.TOTAL_COLLECT, dataRetrived.get(ReportConstants.FREIGHT_AMOUNT_TEXT_C));
        }
    }

    private Map<String, Object> getDocumentDataForReports(IReport report, ReportRequest reportRequest) throws RunnerException {
        Map<String, Object> dataRetrived;
        if (report instanceof PickupOrderReport pickupOrderReport && StringUtility.isNotEmpty(reportRequest.getTransportInstructionId())) {
            dataRetrived = pickupOrderReport.getData(Long.parseLong(reportRequest.getReportId()), Long.parseLong(reportRequest.getTransportInstructionId()));
        } else if (report instanceof DeliveryOrderReport vDeliveryOrderReport) {
            dataRetrived = getDataRetrivedForDeliveryOrderReport(report, reportRequest, vDeliveryOrderReport);
        } else if (report instanceof TransportOrderReport transportOrderReport && StringUtility.isNotEmpty(reportRequest.getTransportInstructionId())) {
            dataRetrived = transportOrderReport.getData(Long.parseLong(reportRequest.getReportId()), Long.parseLong(reportRequest.getTransportInstructionId()));
        } else if (report instanceof HblReport vHblReport) {
            dataRetrived = getDataRetrivedForHblReport(report, reportRequest, vHblReport);
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
        return dataRetrived;
    }

    private Map<String, Object> getDataRetrivedForHblReport(IReport report, ReportRequest reportRequest, HblReport vHblReport) throws RunnerException {
        Map<String, Object> dataRetrived;
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
        return dataRetrived;
    }

    private Map<String, Object> getDataRetrivedForDeliveryOrderReport(IReport report, ReportRequest reportRequest, DeliveryOrderReport vDeliveryOrderReport) throws RunnerException {
        Map<String, Object> dataRetrived;
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
        return dataRetrived;
    }

    private boolean getWaterMarkRequired(ReportRequest reportRequest) {
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
        return waterMarkRequired;
    }

    private byte[] getPdfBytesForHouseBill(ReportRequest reportRequest, Map<String, Object> dataRetrived, boolean waterMarkRequired, byte[] pdfByteContent, BaseFont font, Boolean isOriginalPrint, Boolean isSurrenderPrint, Boolean isNeutralPrint, ShipmentSettingsDetails tenantSettingsRow) throws IOException, DocumentException {
        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HOUSE_BILL))
        {
            boolean disableOriginal = (Boolean) dataRetrived.getOrDefault(ReportConstants.DISABLE_ORIGINAL, false);
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(Long.parseLong(reportRequest.getReportId()));
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent()) {
                shipmentDetails = shipmentsRow.get();
            }
            if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL) && disableOriginal) {
                throw new ValidationException("HBl Original generation is disabled");
            }
            if (reportRequest.getPrintType().equalsIgnoreCase(DRAFT) && Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPrintedOriginal()))
                throw new ValidationException("HBl Draft can't be generated once Original is printed");
            pdfByteContent = getPdfByteContent(reportRequest, waterMarkRequired, pdfByteContent, font, shipmentDetails);
            if (reportRequest.getPrintType().equalsIgnoreCase(TypeOfHblPrint.Original.name()))
            {
                shipmentDetails.getAdditionalDetails().setPrintedOriginal(true);
            }

            updateShipmentDetailsForPrint(dataRetrived, isOriginalPrint, isSurrenderPrint, isNeutralPrint, shipmentDetails);

            shipmentDetails = shipmentDao.update(shipmentDetails, false);
            dependentServiceHelper.pushShipmentDataToDependentService(shipmentDetails, false, false, Optional.ofNullable(shipmentDetails).map(ShipmentDetails::getContainersList).orElse(null));
            try {
                shipmentSync.sync(shipmentDetails, null, null, UUID.randomUUID().toString(), false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
            addHBLToRepoForByteContent(pdfByteContent, reportRequest, tenantSettingsRow, shipmentDetails);
        }
        return pdfByteContent;
    }

    private byte[] getPdfByteContent(ReportRequest reportRequest, boolean waterMarkRequired, byte[] pdfByteContent, BaseFont font, ShipmentDetails shipmentDetails) throws IOException, DocumentException {
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
        return pdfByteContent;
    }

    private void updateShipmentDetailsForPrint(Map<String, Object> dataRetrived, Boolean isOriginalPrint, Boolean isSurrenderPrint, Boolean isNeutralPrint, ShipmentDetails shipmentDetails) {
        if (isOriginalPrint || isSurrenderPrint || Boolean.TRUE.equals(isNeutralPrint)) {
            if(Boolean.TRUE.equals(isOriginalPrint)){
                shipmentDetails.getAdditionalDetails().setPrintedOriginal(true);
            }
            if(ReportConstants.AIR.equalsIgnoreCase(dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString()) && (isOriginalPrint || isNeutralPrint)){
                shipmentDetails.setStatus(ShipmentStatus.GenerateHAWB.getValue());
            }else if(ReportConstants.SEA.equalsIgnoreCase(dataRetrived.get(ReportConstants.TRANSPORT_MODE).toString()) && (isOriginalPrint || isSurrenderPrint)){
                shipmentDetails.setStatus(ShipmentStatus.GenerateHBL.getValue());
            }
        }
    }

    private void addHBLToRepoForByteContent(byte[] pdfByteContent, ReportRequest reportRequest, ShipmentSettingsDetails tenantSettingsRow, ShipmentDetails shipmentDetails) {
        if (pdfByteContent != null)
        {
            String documentType = ReportConstants.SHIPMENT_HOUSE_BILL;
            if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL)) {
                documentType = ReportConstants.ORIGINAL_HOUSE_BILL;
            } else if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)) {
                documentType = ReportConstants.DRAFT_HOUSE_BILL;
            }
            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS);
            docUploadRequest.setId(Long.parseLong(reportRequest.getReportId()));
            docUploadRequest.setDocType(documentType);
            docUploadRequest.setReportId(reportRequest.getReportId());
            try {
                addHouseBillToRepo(docUploadRequest, reportRequest.getPrintType(), pdfByteContent, tenantSettingsRow, shipmentDetails.getAdditionalDetails().getReleaseType(), StringUtility.convertToString(shipmentDetails.getGuid()));
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }
    }

    private void addHBLToRepoForSeawayBill(ReportRequest reportRequest, byte[] pdfByteContent, ShipmentSettingsDetails tenantSettingsRow) {
        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SEAWAY_BILL) && pdfByteContent != null)
        {
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(Long.parseLong(reportRequest.getReportId()));
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent())
                shipmentDetails = shipmentsRow.get();
            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(Constants.SHIPMENTS_WITH_SQ_BRACKETS);
            docUploadRequest.setId(Long.parseLong(reportRequest.getReportId()));
            docUploadRequest.setDocType(ReportConstants.SEAWAY_BILL);
            docUploadRequest.setReportId(reportRequest.getReportId());
            try {
                addHouseBillToRepo(docUploadRequest, TypeOfHblPrint.Draft.name().toUpperCase(), pdfByteContent, tenantSettingsRow, null, StringUtility.convertToString(shipmentDetails.getGuid()));
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }
    }

    private void createEventsForReportInfo(ReportRequest reportRequest, byte[] pdfByteContent, Map<String, Object> dataRetrived, ShipmentSettingsDetails tenantSettingsRow) {
        createEventsForShippingRequest(reportRequest, pdfByteContent, dataRetrived, tenantSettingsRow);

        if (ObjectUtils.isNotEmpty(reportRequest.getReportInfo())) {
            String reportInfo = reportRequest.getReportInfo().toUpperCase();

            if (reportInfo.equals(ReportConstants.PICKUP_ORDER.toUpperCase())) {
                createAutoEvent(reportRequest.getReportId(), ReportConstants.PICKUP_ORDER_GEN, tenantSettingsRow);
            }

            if (reportInfo.equals(ReportConstants.SHIPMENT_CAN_DOCUMENT.toUpperCase())) {
                createEvent(reportRequest, EventConstants.CANG);
            }
        }
    }

    private void createEventsForShippingRequest(ReportRequest reportRequest, byte[] pdfByteContent, Map<String, Object> dataRetrived, ShipmentSettingsDetails tenantSettingsRow) {
        if (reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SHIPPING_REQUEST )&& pdfByteContent != null)
        {
            String shipmentIds = dataRetrived.get(ReportConstants.SHIPMENT_IDS).toString();
            if (StringUtility.isNotEmpty(shipmentIds)) {
                List<String> shipmentIdList = Arrays.stream(shipmentIds.split(",")).toList();
                if (!CommonUtils.listIsNullOrEmpty(shipmentIdList)) {
                    for(String shipmentId : shipmentIdList) {
                        createAutoEvent(shipmentId, EventConstants.SR_SENT_OR_NOT, tenantSettingsRow);
                    }
                }
            }
        }
    }

    private void validateDpsForMawbReport(IReport report, ReportRequest reportRequest, Boolean isOriginalPrint) {
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
    }

    private byte[] getCombinedDataForCargoManifestAir(ReportRequest reportRequest) throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException {
        if((Objects.equals(reportRequest.getReportInfo(), ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION)
                || Objects.equals(reportRequest.getReportInfo(), ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION))
                && reportRequest.isFromConsolidation()) {
            Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(Long.valueOf(reportRequest.getReportId()));
            if(optionalConsolidationDetails.isPresent()) {
                ConsolidationDetails consolidationDetails = optionalConsolidationDetails.get();
                reportRequest.setSelfCall(true);
                List<byte[]> dataByteList = getDataByteList(reportRequest, consolidationDetails);
                reportRequest.setSelfCall(false);
                return CommonUtils.concatAndAddContent(dataByteList);
            }
        }
        return null;
    }

    private List<byte[]> getDataByteList(ReportRequest reportRequest, ConsolidationDetails consolidationDetails) throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException {
        byte[] dataByte;
        List<byte[]> dataByteList = new ArrayList<>();
        Map<String, List<Long>> groupedShipments;
        if(consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
            groupedShipments = consolidationDetails.getShipmentsList().stream()
                    .collect(Collectors.groupingBy(e -> e.getCarrierDetails().getDestinationPort(),
                            Collectors.mapping(ShipmentDetails::getId, Collectors.toList())));
            if(!groupedShipments.isEmpty()) {
                for (Map.Entry<String, List<Long>> entry: groupedShipments.entrySet()) {
                    reportRequest.setFromConsolidation(false);
                    reportRequest.setShipmentIds(entry.getValue());
                    dataByte = self.getDocumentData(CommonRequestModel.buildRequest(reportRequest));
                    if(dataByte != null) {
                        dataByteList.add(dataByte);
                    }
                }
            }

        }
        return dataByteList;
    }

    private void validateOriginalAwbPrintForLinkedShipment(ReportRequest reportRequest) throws RunnerException {
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
    }

    private byte[] getDataForCombinedReport(ReportRequest reportRequest) throws DocumentException, IOException, RunnerException, ExecutionException, InterruptedException {
        if((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.CARGO_MANIFEST) || reportRequest.getReportInfo().equalsIgnoreCase( ReportConstants.SHIPMENT_CAN_DOCUMENT) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.SHIPPING_INSTRUCTION)) && reportRequest.isFromConsolidation()) {
            Optional<ConsolidationDetails> optionalConsolidationDetails = consolidationDetailsDao.findById(Long.valueOf(reportRequest.getReportId()));
            if(optionalConsolidationDetails.isPresent()) {
                ConsolidationDetails consolidationDetails = optionalConsolidationDetails.get();
                byte[] dataByte;
                List<byte[]> dataByteList = new ArrayList<>();
                for(ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                    reportRequest.setFromConsolidation(false);
                    reportRequest.setReportId(shipmentDetails.getId().toString());
                    dataByte = self.getDocumentData(CommonRequestModel.buildRequest(reportRequest));
                    if(dataByte != null) {
                        dataByteList.add(dataByte);
                    }
                }
                return CommonUtils.concatAndAddContent(dataByteList);
            }
        }
        return null;
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
        updatePrintCustomLabelFields(reportRequest, dataRetrived);

        int copies = reportRequest.getCopyCountForAWB() != null ? reportRequest.getCopyCountForAWB() : 0;
        if(copies < 1) throw new ValidationException("Copy count is less than 1");
        Integer noOfPacks = getNoOfPacks(reportRequest, dataRetrived);
        boolean isCombi = dataRetrived.containsKey(ReportConstants.IS_COMBI) && Boolean.TRUE.equals(dataRetrived.get(ReportConstants.IS_COMBI));
        List<Pair<String, Integer>> hawbPacksMap = null; // used only for combi label
        if(isCombi) {
            hawbPacksMap = new ArrayList<>((List<Pair<String, Integer>>) dataRetrived.get(HAWB_PACKS_MAP));
            dataRetrived.remove(HAWB_PACKS_MAP);
            noOfPacks = (Integer) dataRetrived.get(ReportConstants.TOTAL_CONSOL_PACKS);
        }
        if(noOfPacks == null || noOfPacks == 0) {
            throw new ValidationException("no of pack is less than 1");
        }
        processCopies(reportRequest, pages, dataRetrived, pdfBytes, copies, noOfPacks, isCombi, hawbPacksMap);
    }

    private void processCopies(ReportRequest reportRequest, DocPages pages, Map<String, Object> dataRetrived, List<byte[]> pdfBytes, int copies, Integer noOfPacks, boolean isCombi, List<Pair<String, Integer>> hawbPacksMap) {
        for(int i = 1; i <= copies; i++) {
            int ind = 0;
            int prevPacks = 0;
            List<Future<byte[]>> futures = new ArrayList<>();
            for (int packs = 1; packs <= noOfPacks; packs++) {
                Map<String, Object> threadSafeData = new HashMap<>(dataRetrived);
                String packsCount = getSerialCount(packs, copies);
                String packsOfTotal = packs + "/" + noOfPacks;
                String hawbPacksCountForCombi;
                if(isCombi) {
                    threadSafeData.put(ReportConstants.HAWB_NUMBER, hawbPacksMap.get(ind).getKey());
                    packsOfTotal = (packs - prevPacks) + "/" + hawbPacksMap.get(ind).getValue();
                    hawbPacksCountForCombi = getSerialCount(packs - prevPacks, copies);
                    threadSafeData.put(COMBI_HAWB_COUNT, hawbPacksCountForCombi);
                    if((packs-prevPacks)% hawbPacksMap.get(ind).getValue() == 0) {
                        prevPacks = prevPacks + hawbPacksMap.get(ind).getValue();
                        ind++;
                    }
                } else {
                  hawbPacksCountForCombi = "";
                }
                populateMap(threadSafeData, packsCount, packsOfTotal, packs);

                int finalPacks = packs;
                log.info("Submitting task for Pack: {} on Thread: {}", finalPacks, Thread.currentThread().getName());
                futures.add(executorServiceReport.submit(() -> {
                    log.info("Started processing Pack: {} on Thread: {}", finalPacks, Thread.currentThread().getName());
                    long start = System.currentTimeMillis();

                    byte[] result = addDocBytesInPdfBytes(reportRequest, pages, threadSafeData, isCombi, packsCount, hawbPacksCountForCombi);

                    long end = System.currentTimeMillis();
                    log.info("Completed Pack: {} on Thread: {} in {} ms", finalPacks, Thread.currentThread().getName(), (end - start));
                    return result;
                }));
            }

            populatePdfBytes(futures, pdfBytes);
        }
    }

    private void populatePdfBytes(List<Future<byte[]>> futures, List<byte[]> pdfBytes){
        for (Future<byte[]> future : futures) {
            try {
                pdfBytes.add(future.get());
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                throw new GenericException(e.getMessage());
            }
        }
    }

    private void populateMap(Map<String, Object> threadSafeData, String packsCount, String packsOfTotal, int packs){
        if (threadSafeData.get(ReportConstants.MAWB_NUMBER) != null || threadSafeData.get(ReportConstants.HAWB_NUMBER) != null) {
            threadSafeData.put(ReportConstants.COUNT, packsCount);
            threadSafeData.put(ReportConstants.PACKS_OF_TOTAL, packsOfTotal);
            threadSafeData.put(ReportConstants.PACK_NUMBER, packs);
        }
        else {
            threadSafeData.put(ReportConstants.COUNT, null);
        }
    }

    private void updatePrintCustomLabelFields(ReportRequest reportRequest, Map<String, Object> dataRetrived) {
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
    }

    private byte[] addDocBytesInPdfBytes(ReportRequest reportRequest, DocPages pages, Map<String, Object> dataRetrived, boolean isCombi, String packsCount, String hawbPacksCountForCombi) {
        byte[] mainDocPage = getFromDocumentService(dataRetrived, pages.getMainPageId());
        if (mainDocPage == null)
            throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
        String mawbNumber = Constants.EMPTY_STRING;
        String hawbNumber = Constants.EMPTY_STRING;
        if(reportRequest.isFromConsolidation() || dataRetrived.get(ReportConstants.HAWB_NUMBER) == null ||
                StringUtility.isEmpty(dataRetrived.get(ReportConstants.HAWB_NUMBER).toString()) || isCombi)
            mawbNumber = dataRetrived.get(ReportConstants.MAWB_NUMBER) != null ? dataRetrived.get(ReportConstants.MAWB_NUMBER) + packsCount : packsCount;
        else
            hawbNumber = dataRetrived.get(ReportConstants.HAWB_NUMBER) != null ? dataRetrived.get(ReportConstants.HAWB_NUMBER) + packsCount : packsCount;
        byte[] docBytes = addBarCodeInAWBLableReport(mainDocPage, mawbNumber, hawbNumber);
        if(isCombi) {
            docBytes = addBarCodeForCombiReport(docBytes, dataRetrived.get(ReportConstants.HAWB_NUMBER) != null ? dataRetrived.get(ReportConstants.HAWB_NUMBER) + hawbPacksCountForCombi : hawbPacksCountForCombi);
        }
       return docBytes;
    }

    private Integer getNoOfPacks(ReportRequest reportRequest, Map<String, Object> dataRetrived) {
        Integer noOfPacks = 0;
        if(reportRequest.isFromConsolidation() && dataRetrived.get(ReportConstants.TOTAL_CONSOL_PACKS) != null) {
            noOfPacks = (Integer) dataRetrived.get(ReportConstants.TOTAL_CONSOL_PACKS);
        } else if (dataRetrived.get(ReportConstants.TOTAL_PACKS) != null) {
            noOfPacks = (Integer) dataRetrived.get(ReportConstants.TOTAL_PACKS);
        }
        return noOfPacks;
    }

    public byte[] getFromDocumentService(Object json, String templateId) {
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


    public DocPages getFromTenantSettings(String key, String objectType, String printType, String frontTemplateCode, String  backTemplateCode,
                                          String transportMode, boolean isTransportInstruction)
    {
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(1, UserContext.getUser().TenantId));
        if (shipmentSettingsDetailsList != null && !shipmentSettingsDetailsList.isEmpty())
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
            DocPages page = getTemplateId(tenant, admin, key, objectType,
                    printType, frontTemplateCode, backTemplateCode, transportMode, isTransportInstruction);
            if (page != null && Strings.isNullOrEmpty(page.getFirstPageId()) && Strings.isNullOrEmpty(page.getMainPageId()) && Strings.isNullOrEmpty(page.getBackPrintId())) {
                throw new ValidationException("Please upload template in branch settings for: " + key);
            }
            return page;

        }
        return null;
    }

    public static String getMainOrLastPageId(String value, String adminRowValue) {
        return value == null ? adminRowValue : value;
    }

    public static boolean getIsLogoFixed(String value) {
        return value!=null;
    }


    public DocPages getTemplateId(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String docKey, String objectType, String printType,
                                  String frontTemplateCode, String backTemplateCode, String transportMode, boolean istransportInstruction) {
        // Group similar document types that use the same pattern
        if (isSimpleDocument(docKey)) {
            return handleSimpleDocument(row, adminRow, docKey);
        }

        return switch (docKey) {
            case ReportConstants.PACKING_LIST -> setDocPagesForPackingList(row, adminRow, objectType);
            case ReportConstants.CUSTOMS_INSTRUCTION -> setDocPagesForCustomsInstruction(row, adminRow, objectType);
            case ReportConstants.SHIPMENT_CAN_DOCUMENT -> setDocPagesForCanMainPageAir(row, adminRow, objectType);
            case ReportConstants.SHIPMENT_HOUSE_BILL ->
                    setDocPagesForHouseBill(row, adminRow, printType, frontTemplateCode, backTemplateCode);
            case ReportConstants.ARRIVAL_NOTICE -> setDocPagesForArrivalNotice(row, adminRow, objectType);
            case ReportConstants.FREIGHT_CERTIFICATION -> setDocPagesForFreightCertification(row, adminRow, objectType);
            case ReportConstants.PRE_ALERT -> setDocPagesForPreAlert(row, adminRow, objectType);
            case ReportConstants.PICKUP_ORDER ->
                    setDocPagesForPickupOrder(row, adminRow, objectType, istransportInstruction);
            case ReportConstants.DELIVERY_ORDER ->
                    setDocPagesForDeliveryOrder(row, adminRow, objectType, istransportInstruction);
            case ReportConstants.BOOKING_CONFIRMATION -> setDocPagesForBookingConfirmation(row, adminRow, objectType);
            case ReportConstants.SHIPPING_INSTRUCTION -> getShippingInstructionDocument(row, adminRow, objectType);
            case ReportConstants.IMPORT_SHIPMENT_MANIFEST ->
                    setDocPagesForImportShipmentManifest(row, adminRow, objectType);
            case ReportConstants.EXPORT_SHIPMENT_MANIFEST ->
                    setDocPagesForExportShipmentManifest(row, adminRow, objectType);
            case ReportConstants.IMPORT_CONSOL_MANIFEST ->
                    setDocPagesForImportConsolManifest(row, adminRow, objectType);
            case ReportConstants.EXPORT_CONSOL_MANIFEST ->
                    setDocPagesForExportConsolManifest(row, adminRow, objectType);
            case ReportConstants.COMMERCIAL_INVOICE -> setDocPagesForCommercialInvoice(row, adminRow, objectType);
            case ReportConstants.BOOKING_ORDER -> setDocPagesForBookingOrder(row, adminRow, transportMode);
            default -> null;
        };
    }

    private boolean isSimpleDocument(String docKey) {
        return docKey.equals(ReportConstants.SEAWAY_BILL) ||
                docKey.equals(ReportConstants.SHIP_TRUCKWAY_BILL) ||
                docKey.equals(ReportConstants.CONS_TRUCKWAY_BILL) ||
                docKey.equals(ReportConstants.SHIP_TRUCK_DRIVER_PROOF) ||
                docKey.equals(ReportConstants.CONS_TRUCK_DRIVER_PROOF) ||
                docKey.equals(ReportConstants.AIRWAY_BILL) ||
                docKey.equals(ReportConstants.PROOF_OF_DELIVERY) ||
                docKey.equals(ReportConstants.COSTAL_DOC) ||
                docKey.equals(ReportConstants.AWB_LABEL) ||
                docKey.equals(ReportConstants.CARGO_MANIFEST) ||
                docKey.equals(ReportConstants.CONSOLIDATED_PACKING_LIST) ||
                docKey.equals(ReportConstants.HAWB) ||
                docKey.equals(ReportConstants.MAWB) ||
                docKey.equals(ReportConstants.AWB_NEUTRAL) ||
                docKey.equals(ReportConstants.SHIPPING_REQUEST) ||
                docKey.equals(ReportConstants.SHIPPING_REQUEST_AIR) ||
                docKey.equals(ReportConstants.CARGO_MANIFEST_AIR_IMPORT_SHIPMENT) ||
                docKey.equals(ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION) ||
                docKey.equals(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT) ||
                docKey.equals(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION) ||
                docKey.equals(ReportConstants.CSR) ||
                docKey.equals(ReportConstants.GENERATE_ISF_FILE) ||
                docKey.equals(ReportConstants.CONTAINER_MANIFEST_PRINT) ||
                docKey.equals(ReportConstants.MANIFEST_PRINT) ||
                docKey.equals(ReportConstants.TRANSPORT_ORDER) ||
                docKey.equals(ReportConstants.CSD_REPORT) ||
                docKey.equals(ReportConstants.FCR_DOCUMENT);
    }

    @SuppressWarnings("java:S3516")
    private DocPages handleSimpleDocument(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String docKey) {
        String mainPageId;
        boolean isLogoFixed;

        switch (docKey) {
            case ReportConstants.SEAWAY_BILL:
                mainPageId = getMainOrLastPageId(row.getSeawayMainPage(), adminRow.getSeawayMainPage());
                isLogoFixed = getIsLogoFixed(row.getSeawayMainPage());
                break;
            case ReportConstants.SHIP_TRUCKWAY_BILL:
                mainPageId = getMainOrLastPageId(row.getShipTruckWayBillMainPage(), adminRow.getShipTruckWayBillMainPage());
                isLogoFixed = getIsLogoFixed(row.getShipTruckWayBillMainPage());
                break;
            case ReportConstants.CONS_TRUCKWAY_BILL:
                mainPageId = getMainOrLastPageId(row.getConsTruckWayBillMainPage(), adminRow.getConsTruckWayBillMainPage());
                isLogoFixed = getIsLogoFixed(row.getConsTruckWayBillMainPage());
                break;
            case ReportConstants.SHIP_TRUCK_DRIVER_PROOF:
                mainPageId = getMainOrLastPageId(row.getShipTruckDriverProof(), adminRow.getShipTruckDriverProof());
                isLogoFixed = getIsLogoFixed(row.getShipTruckDriverProof());
                break;
            case ReportConstants.CONS_TRUCK_DRIVER_PROOF:
                mainPageId = getMainOrLastPageId(row.getConsTruckDriverProof(), adminRow.getConsTruckDriverProof());
                isLogoFixed = getIsLogoFixed(row.getConsTruckDriverProof());
                break;
            case ReportConstants.AIRWAY_BILL:
                mainPageId = getMainOrLastPageId(row.getAirwayMainPage(), adminRow.getAirwayMainPage());
                isLogoFixed = getIsLogoFixed(row.getAirwayMainPage());
                break;
            case ReportConstants.PROOF_OF_DELIVERY:
                mainPageId = getMainOrLastPageId(row.getProofOfDelivery(), adminRow.getProofOfDelivery());
                isLogoFixed = getIsLogoFixed(row.getProofOfDelivery());
                break;
            case ReportConstants.COSTAL_DOC:
                mainPageId = getMainOrLastPageId(row.getCostalDocument(), adminRow.getCostalDocument());
                isLogoFixed = getIsLogoFixed(row.getCostalDocument());
                break;
            case ReportConstants.AWB_LABEL:
                mainPageId = getMainOrLastPageId(row.getAwbLable(), adminRow.getAwbLable());
                isLogoFixed = getIsLogoFixed(row.getAwbLable());
                break;
            case ReportConstants.CARGO_MANIFEST:
                mainPageId = getMainOrLastPageId(row.getCargoManifest(), adminRow.getCargoManifest());
                isLogoFixed = getIsLogoFixed(row.getCargoManifest());
                break;
            case ReportConstants.CONSOLIDATED_PACKING_LIST:
                mainPageId = getMainOrLastPageId(row.getConsolidatedPackingList(), adminRow.getConsolidatedPackingList());
                isLogoFixed = getIsLogoFixed(row.getConsolidatedPackingList());
                break;
            case ReportConstants.HAWB:
                mainPageId = getMainOrLastPageId(row.getHawb(), adminRow.getHawb());
                isLogoFixed = getIsLogoFixed(row.getHawb());
                break;
            case ReportConstants.MAWB:
                mainPageId = getMainOrLastPageId(row.getMawb(), adminRow.getMawb());
                isLogoFixed = getIsLogoFixed(row.getMawb());
                break;
            case ReportConstants.AWB_NEUTRAL:
                mainPageId = getMainOrLastPageId(row.getAwbNeutral(), adminRow.getMawb());
                isLogoFixed = getIsLogoFixed(row.getAwbNeutral());
                break;
            case ReportConstants.SHIPPING_REQUEST:
                mainPageId = getMainOrLastPageId(row.getShippingRequestMainPage(), adminRow.getShippingRequestMainPage());
                isLogoFixed = getIsLogoFixed(row.getShippingRequestMainPage());
                break;
            case ReportConstants.SHIPPING_REQUEST_AIR:
                mainPageId = getMainOrLastPageId(row.getShippingRequestAir(), adminRow.getShippingRequestAir());
                isLogoFixed = getIsLogoFixed(row.getShippingRequestAir());
                break;
            case ReportConstants.CARGO_MANIFEST_AIR_IMPORT_SHIPMENT:
                mainPageId = getMainOrLastPageId(row.getAirImportShipmentManifest(), adminRow.getAirImportShipmentManifest());
                isLogoFixed = getIsLogoFixed(row.getAirImportShipmentManifest());
                break;
            case ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION:
                mainPageId = getMainOrLastPageId(row.getAirImportConsoleManifest(), adminRow.getAirImportConsoleManifest());
                isLogoFixed = getIsLogoFixed(row.getAirImportConsoleManifest());
                break;
            case ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT:
                mainPageId = getMainOrLastPageId(row.getAirExportShipmentManifest(), adminRow.getAirExportShipmentManifest());
                isLogoFixed = getIsLogoFixed(row.getAirExportShipmentManifest());
                break;
            case ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION:
                mainPageId = getMainOrLastPageId(row.getAirExportConsoleManifest(), adminRow.getAirExportConsoleManifest());
                isLogoFixed = getIsLogoFixed(row.getAirExportConsoleManifest());
                break;
            case ReportConstants.CSR:
                mainPageId = getMainOrLastPageId(row.getCsr(), adminRow.getCsr());
                isLogoFixed = getIsLogoFixed(row.getCsr());
                break;
            case ReportConstants.GENERATE_ISF_FILE:
                mainPageId = getMainOrLastPageId(row.getIsfFileMainPage(), adminRow.getIsfFileMainPage());
                isLogoFixed = getIsLogoFixed(row.getIsfFileMainPage());
                break;
            case ReportConstants.CONTAINER_MANIFEST_PRINT:
                mainPageId = getMainOrLastPageId(row.getContainerManifestPrint(), adminRow.getContainerManifestPrint());
                isLogoFixed = getIsLogoFixed(row.getContainerManifestPrint());
                break;
            case ReportConstants.MANIFEST_PRINT:
                mainPageId = getMainOrLastPageId(row.getManifestPrint(), adminRow.getManifestPrint());
                isLogoFixed = getIsLogoFixed(row.getManifestPrint());
                break;
            case ReportConstants.TRANSPORT_ORDER:
                mainPageId = getMainOrLastPageId(row.getTransportOrderRoad(), adminRow.getTransportOrderRoad());
                isLogoFixed = getIsLogoFixed(row.getTransportOrderRoad());
                break;
            case ReportConstants.CSD_REPORT:
                mainPageId = getMainOrLastPageId(row.getCsd(), adminRow.getCsd());
                isLogoFixed = getIsLogoFixed(row.getCsd());
                break;
            case ReportConstants.FCR_DOCUMENT:
                mainPageId = getMainOrLastPageId(row.getFcrDocument(), adminRow.getFcrDocument());
                isLogoFixed = getIsLogoFixed(row.getFcrDocument());
                break;
            default:
                return null;
        }

        return setDocPages(null, mainPageId, null, isLogoFixed, null, null, null);
    }

    private DocPages setDocPagesForPreAlert(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null, getMainOrLastPageId(row.getPreAlertAir(), adminRow.getPreAlertAir()), null, getIsLogoFixed(row.getPreAlertAir()), null, null, null);
        }else{
            return setDocPages(null, getMainOrLastPageId(row.getPreAlertDoc(), adminRow.getPreAlertDoc()), null, getIsLogoFixed(row.getPreAlertDoc()), null, null, null);
        }
    }

    private DocPages setDocPagesForFreightCertification(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null, getMainOrLastPageId(row.getFreightCertificationAir(), adminRow.getFreightCertificationAir()), null, true, null, null, null);
        }else{
            return setDocPages(null, getMainOrLastPageId(row.getFreightCertification(), adminRow.getFreightCertification()), null, true, null, null, null);
        }
    }

    private DocPages setDocPagesForArrivalNotice(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null, getMainOrLastPageId(row.getArrivalNoticeAir(), adminRow.getArrivalNoticeAir()), null, getIsLogoFixed(row.getArrivalNoticeAir()), null, null, null);
        } else {
            return setDocPages(null, getMainOrLastPageId(row.getArrivalNotice(), adminRow.getArrivalNotice()), null, getIsLogoFixed(row.getArrivalNotice()), null, null, null);
        }
    }

    private DocPages setDocPagesForCustomsInstruction(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null, getMainOrLastPageId(row.getCustomsInsMainPageAir(), adminRow.getCustomsInsMainPageAir()), null, getIsLogoFixed(row.getCustomsInsMainPageAir()), null, null, null);
        }else{
            return setDocPages(null, getMainOrLastPageId(row.getCustomsInsMainPage(), adminRow.getCustomsInsMainPage()), null, getIsLogoFixed(row.getCustomsInsMainPage()), null, null, null);
        }
    }

    private DocPages setDocPagesForPackingList(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null, getMainOrLastPageId(row.getPackingListMainPageAir(), adminRow.getPackingListMainPageAir()), null, getIsLogoFixed(row.getPackingListMainPageAir()), null, null, null);
        }else{
            return setDocPages(null, getMainOrLastPageId(row.getPackingListMainPage(), adminRow.getPackingListMainPage()), null, getIsLogoFixed(row.getPackingListMainPage()), null, null, null);
        }
    }

    private DocPages setDocPagesForImportShipmentManifest(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
            return setDocPages(null,
                    row.getAirImportShipmentManifest() == null ? adminRow.getAirImportShipmentManifest() : row.getAirImportShipmentManifest(), null, row.getAirImportShipmentManifest() != null, null, null, null);
        } else {
            return setDocPages(null,
                    row.getSeaImportShipmentManifest() == null ? adminRow.getSeaImportShipmentManifest() : row.getSeaImportShipmentManifest(), null, row.getSeaImportShipmentManifest() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForExportShipmentManifest(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
            return setDocPages(null,
                    row.getAirExportShipmentManifest() == null ? adminRow.getAirExportShipmentManifest() : row.getAirExportShipmentManifest(), null, row.getAirExportShipmentManifest() != null, null, null, null);
        } else {
            return setDocPages(null,
                    row.getSeaExportShipmentManifest() == null ? adminRow.getSeaExportShipmentManifest() : row.getSeaExportShipmentManifest(), null, row.getSeaExportShipmentManifest() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForImportConsolManifest(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
            return setDocPages(null,
                    row.getAirImportConsoleManifest() == null ? adminRow.getAirImportConsoleManifest() : row.getAirImportConsoleManifest(), null, row.getAirImportConsoleManifest() != null, null, null, null);
        } else {
            return setDocPages(null,
                    row.getSeaImportConsoleManifest() == null ? adminRow.getSeaImportConsoleManifest() : row.getSeaImportConsoleManifest(), null, row.getSeaImportConsoleManifest() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForExportConsolManifest(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR)) {
            return setDocPages(null,
                    row.getAirExportConsoleManifest() == null ? adminRow.getAirExportConsoleManifest() : row.getAirExportConsoleManifest(), null, row.getAirExportConsoleManifest() != null, null, null, null);
        } else {
            return setDocPages(null,
                    row.getSeaExportConsoleManifest() == null ? adminRow.getSeaExportConsoleManifest() : row.getSeaExportConsoleManifest(), null, row.getSeaExportConsoleManifest() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForCommercialInvoice(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType != null && objectType.equalsIgnoreCase(ReportConstants.TRANS_AIR))
            return setDocPages(null,
                    row.getCommercialInvMainPageAir() == null ? adminRow.getCommercialInvMainPageAir() : row.getCommercialInvMainPageAir(), null, row.getCommercialInvMainPageAir() != null, null, null, null);
        else
            return setDocPages(null,
                    row.getCommercialInvMainPage() == null ? adminRow.getCommercialInvMainPage() : row.getCommercialInvMainPage(), null, row.getCommercialInvMainPage() != null, null, null, null);
    }

    private DocPages setDocPagesForBookingOrder(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String transportMode) {
        if (transportMode.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null,
                    row.getBookingOrderAir() == null ? adminRow.getBookingOrderAir() : row.getBookingOrderAir(), null, row.getBookingOrderAir() != null, null, null, null);
        }else{
            return setDocPages(null,
                    row.getBookingOrder() == null ? adminRow.getBookingOrder() : row.getBookingOrder(), null, row.getBookingOrder() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForBookingConfirmation(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null,
                    row.getBookingConfirmationAir() == null ? adminRow.getBookingConfirmationAir() : row.getBookingConfirmationAir(), null, row.getBookingConfirmationAir() != null, null, null, null);
        }else{
            return setDocPages(null,
                    row.getBookingConfirmation() == null ? adminRow.getBookingConfirmation() : row.getBookingConfirmation(), null, row.getBookingConfirmation() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForDeliveryOrder(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType, boolean istransportInstruction) {
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
    }

    private DocPages setDocPagesForPickupOrder(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType, boolean istransportInstruction) {
        if(istransportInstruction){
            return setDocPages(null,
                    row.getTransportInstructionPickupOrder() == null ? adminRow.getTransportInstructionPickupOrder() : row.getTransportInstructionPickupOrder(), null, row.getTransportInstructionPickupOrder() != null, null, null, null);
        }
        else if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null,
                    row.getPickupOrderAir() == null ? adminRow.getPickupOrderAir() : row.getPickupOrderAir(), null, row.getPickupOrderAir() != null, null, null, null);
        }else{
            return setDocPages(null,
                    row.getPickupOrder() == null ? adminRow.getPickupOrder() : row.getPickupOrder(), null, row.getPickupOrder() != null, null, null, null);
        }
    }

    private DocPages setDocPagesForHouseBill(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String printType, String frontTemplateCode, String backTemplateCode) {
        try
        {
            if (frontTemplateCode != null || backTemplateCode != null)
            {
                String front = hblTermsConditionTemplateDao.getTemplateCode(frontTemplateCode, true , printType).getTemplateFileName();
                String back = null;
                if (StringUtility.isNotEmpty(backTemplateCode))
                {
                    back = hblTermsConditionTemplateDao.getTemplateCode(backTemplateCode, false , printType).getTemplateFileName();
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
    }

    private DocPages setDocPagesForCanMainPageAir(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType) {
        if (objectType.equalsIgnoreCase(ReportConstants.AIR)){
            return setDocPages(null,
                    row.getCanMainPageAir() == null ? adminRow.getCanMainPageAir() : row.getCanMainPageAir(), row.getCanBackPrintAir() == null ? adminRow.getCanBackPrintAir() : row.getCanMainPageAir(), row.getCanMainPageAir() != null, null, null, null);
        }else{
            return setDocPages(null,
                    row.getCanMainPage() == null ? adminRow.getCanMainPage() : row.getCanMainPage(), row.getCanBackPrint() == null ? adminRow.getCanBackPrint() : row.getCanBackPrint(), row.getCanMainPage() != null, null, null, null);
        }
    }

    public static DocPages setDocPages(String firstPageId, String mainPageId, String lastPageId, boolean isLogoFixed,
                                       String multiTemplateCode, String entityType, ShipmentSettingsDetails tenantRow)
    {
        DocPages docPages = new DocPages();
        if (StringUtility.isNotEmpty(multiTemplateCode) && StringUtility.isNotEmpty(entityType)) {
            log.info("Continue the process.");
        } else {
            docPages.setMainPageId(mainPageId);
        }

        docPages.setFirstPageId(firstPageId);
        docPages.setBackPrintId(lastPageId);
        if (docPages.getMainPageId() != null) {
            isLogoFixed = true;
        }
        docPages.setLogoFixed(isLogoFixed);
        docPages.setShipmentSettingsDetails(tenantRow);

        return docPages;
    }

    public DocPages getShippingInstructionDocument(ShipmentSettingsDetails row, ShipmentSettingsDetails adminRow, String objectType)
    {
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
        String copyCount = Integer.toString(copyNumber);
        String totalCopiesStr = Integer.toString(totalCopies);
        StringBuilder ans = new StringBuilder(copyCount);
        int size = copyCount.length();
        if(copyCount.length() < 5 && totalCopiesStr.length() < 5){
            for(int i = 0; i < (5 - size); i++){
                ans = new StringBuilder("0" + ans);
            }
        }else if(copyCount.length() < 5){
            int totalCopiesSize = totalCopiesStr.length();
            for(int i = 0; i< (totalCopiesSize - size); i++){
                ans = new StringBuilder("0" + ans);
            }
        }
        return ans.toString();
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
        if (CommonUtils.hasUnsupportedCharacters(str)) {
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
            image1.setAbsolutePosition(realPageSize.getLeft() + x, realPageSize.getTop() + y);
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
        DocPages docPages = getFromTenantSettings(ReportConstants.AWB_NEUTRAL,null, null, null, null, null,false);
        return getFromDocumentService(json, docPages.getMainPageId());
    }

    public List<byte[]> getOriginalandCopies(DocPages pages, String reportInfo, byte[] mainDoc, byte[] firstpage, byte[] backprint, final Map<String, Object> json, String hbltype, ShipmentSettingsDetails shipmentSettings, String noOfCopies, ReportRequest reportRequest) throws DocumentException, IOException {

        List<byte[]> pdfBytes = new ArrayList<>();

        String logopath = getLogopath(pages, reportInfo, json, hbltype);

        int originalCount = Integer.parseInt(json.getOrDefault(ReportConstants.ORIGINALS, -1).toString());
        int copyCount = Integer.parseInt(json.getOrDefault(ReportConstants.COPY_BILLS, -1).toString());

        if (!reportInfo.equalsIgnoreCase(ReportConstants.SHIPMENT_HOUSE_BILL))
        {
            copyCount = -1;

            byte[] pdfByteContentOriginal = mergeDocumentBytes(mainDoc, firstpage, backprint, logopath, reportInfo, pages.getShipmentSettingsDetails());
            pdfBytes.add(pdfByteContentOriginal);
        }
        else if ( originalCount != 0 )
        {
            addPdfBytesForOriginalCount(pages, reportInfo, firstpage, backprint, json, reportRequest, originalCount, logopath, pdfBytes);

        }

        mainDoc = getBytesForMainDoc(pages, reportInfo, mainDoc, firstpage, backprint, json, reportRequest, copyCount, logopath, pdfBytes);

        try {
            if (Boolean.TRUE.equals(!Objects.isNull(shipmentSettings) && !Objects.isNull(shipmentSettings.getRestrictBlRelease())
                    && shipmentSettings.getRestrictBlRelease()) && StringUtility.isNotEmpty(noOfCopies)) {
                int copy = Integer.parseInt(noOfCopies);
                byte[] pdfByteContentCopy = mergeDocumentBytes(mainDoc, firstpage, backprint, logopath, reportInfo, pages.getShipmentSettingsDetails());
                while (copy-- > 1) {
                    pdfBytes.add(pdfByteContentCopy);
                }
            }
        } catch (Exception ex) { /* Ignore */ }
        return pdfBytes;

    }

    private byte[] getBytesForMainDoc(DocPages pages, String reportInfo, byte[] mainDoc, byte[] firstpage, byte[] backprint, Map<String, Object> json, ReportRequest reportRequest, int copyCount, String logopath, List<byte[]> pdfBytes) throws DocumentException, IOException {
        if (copyCount > 0)
        {
            json.put(ReportConstants.ORIGINAL_OR_COPY, ReportConstants.COPY);
            json.put(ReportConstants.CHARGES, json.get(ReportConstants.COPY_CHARGES));
            json.put(ReportConstants.AS_AGREED, json.get(ReportConstants.COPY_AS_AGREED));
            if(reportRequest.isPrintForParties()){
                mainDoc = printForPartiesAndBarcode(reportRequest, new ArrayList<>(), json.get(ReportConstants.HAWB_NO) == null? "" : json.get(ReportConstants.HAWB_NO).toString(), json, pages);
            }else{
                mainDoc = getFromDocumentService(json, pages.getMainPageId());
            }
            byte[] pdfByteContentCopy = mergeDocumentBytes(mainDoc, firstpage, backprint, logopath, reportInfo, pages.getShipmentSettingsDetails());
            for (int i = 0; i < copyCount; i++)
            {
                pdfBytes.add(pdfByteContentCopy);
            }

        }
        return mainDoc;
    }

    private String getLogopath(DocPages pages, String reportInfo, Map<String, Object> json, String hbltype) {
        String logopath = (String) json.getOrDefault(ReportConstants.LOGO, null);


        if (pages.isLogoFixed() || Boolean.TRUE.equals(isHblType(hbltype, reportInfo)))
        {
            logopath = null;
        }
        return logopath;
    }

    private void addPdfBytesForOriginalCount(DocPages pages, String reportInfo, byte[] firstpage, byte[] backprint, Map<String, Object> json, ReportRequest reportRequest, int originalCount, String logopath, List<byte[]> pdfBytes) throws DocumentException, IOException {
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
                            throw new GenericException(e);
                        }
                    }else{
                        mainDocCurrent = getFromDocumentService(jsonDictClone, pages.getMainPageId());
                    }
                    mainDocParallel.put(i, mainDocCurrent);
                }
        );

        for(int i = 1; i <= originalCount; i++){
            byte[] pdfByteContentCurrent = mergeDocumentBytes(mainDocParallel.get(i), firstpage, backprint, logopath, reportInfo, pages.getShipmentSettingsDetails());
            pdfBytes.add(pdfByteContentCurrent);
        }
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

        if (reportInfo.equalsIgnoreCase(ReportConstants.SHIPMENT_HOUSE_BILL) && Boolean.TRUE.equals(tenantRow.getPrintAfterEachPage())) {
            PdfReader pdfReader = new PdfReader(mainDoc);
            int totalPages = pdfReader.getNumberOfPages();
            for (int i = 1; i <= totalPages; i++) {
                PdfReader pdfReaderMaindoc = new PdfReader(mainDoc);
                pages = new ArrayList<>();
                pages.add(i);
                pdfReaderMaindoc.selectPages(pages);
                pdfConcat.addPages(pdfReaderMaindoc);

                if (backPrint != null) {
                    PdfReader pdfReaderBackdoc = new PdfReader(backPrint);
                    List<Integer> pages2 = new ArrayList<>();
                    pages2.add(1);
                    pdfReaderBackdoc.selectPages(pages2);
                    pdfConcat.addPages(pdfReaderBackdoc);
                }
                pdfReaderMaindoc.close();
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

    public byte[] addImage(byte[] inputBytes, String logopath) {
        if (StringUtility.isEmpty(logopath))
            return inputBytes;
        return inputBytes;
    }

    private void createAutoEvent(String reportId, String eventCode, ShipmentSettingsDetails tenantSettingsRow) {
        if (Boolean.TRUE.equals(tenantSettingsRow.getAutoEventCreate()) && StringUtility.isNotEmpty(reportId)) {
            CustomAutoEventRequest eventReq = new CustomAutoEventRequest();
            eventReq.entityId = Long.parseLong(reportId);
            eventReq.entityType = Constants.SHIPMENT;
            eventReq.eventCode = eventCode;
            eventDao.autoGenerateEvents(eventReq);
        }
    }

    public void addHouseBillToRepo(DocUploadRequest uploadRequest, String printType, byte[] document, ShipmentSettingsDetails shipmentSettingsDetails, String releaseType, String shipmentGuid) {
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
            if (blObject != null && blObject.getHblData() != null && blObject.getHblData().getVersion() != null) {
                fileVersion = blObject.getHblData().getVersion().toString();
                blObject.getHblData().setVersion(blObject.getHblData().getVersion() + 1);
                hblDao.save(blObject);
            }
        }
        String filename = uploadRequest.getDocType() + "_" + printType + "_" + uploadRequest.getId() + "_" + fileVersion + ".pdf";

        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addFilesFromReport(new BASE64DecodedMultipartFile(document), filename, uploadRequest, shipmentGuid)), executorService);

    }

    private void updateInReleaseMappingTable(Hbl hbl, String releaseType, ShipmentSettingsDetails shipmentSettings) {
        if (StringUtility.isNotEmpty(releaseType) && !Objects.isNull(shipmentSettings) && !Objects.isNull(shipmentSettings.getRestrictBlRelease())
                && Boolean.TRUE.equals(shipmentSettings.getRestrictBlRelease())) {
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

    public void addDocumentToDocumentMaster(ReportRequest reportRequest,  byte[] pdfByteContent) {
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

            byte[] finalPdfByteContent = pdfByteContent;
            String documentType = documentTypeFinder(reportRequest);

            DocUploadRequest docUploadRequest = new DocUploadRequest();
            docUploadRequest.setEntityType(isShipment ? Constants.SHIPMENTS_WITH_SQ_BRACKETS : Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS);
            docUploadRequest.setId(Long.parseLong(reportRequest.getReportId()));
            docUploadRequest.setDocType(documentType);
            docUploadRequest.setReportId(reportRequest.getReportId());
            if(reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL))
                docUploadRequest.setIsTransferEnabled(Boolean.TRUE);
            String filename = docUploadRequest.getDocType() + "_" + reportRequest.getPrintType() + "_" + docUploadRequest.getId() + ".pdf";
            String finalGuid = guid;
            CompletableFuture.runAsync(masterDataUtils.withMdc(
                () -> addFilesFromReport(new BASE64DecodedMultipartFile(finalPdfByteContent), filename,
                    docUploadRequest, finalGuid)), executorService);


            if(reportRequest.isPrintCSD() && ReportConstants.ORIGINAL.equalsIgnoreCase(reportRequest.getPrintType())){
                addCSDDocumentToDocumentMaster(reportRequest.getReportId(), docUploadRequest, guid);
            }
        }catch(Exception ex){
            log.error(ex.getMessage());
        }
    }

    public void addCSDDocumentToDocumentMaster(String reportId, DocUploadRequest docUploadRequest, String guid) {
        ReportRequest reportRequest = new ReportRequest();
        reportRequest.setReportId(reportId);
        reportRequest.setReportInfo(CSD_REPORT);
        if(Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS.equalsIgnoreCase(docUploadRequest.getEntityType())){
            reportRequest.setFromConsolidation(true);
        }
        try{
        CommonRequestModel commonRequestModel =  CommonRequestModel.buildRequest(reportRequest);
        DocUploadRequest csdDocumentUploadRequest = new DocUploadRequest(docUploadRequest);
        csdDocumentUploadRequest.setDocType(CSD_REPORT);
        String filename = CSD_REPORT + "_" + docUploadRequest.getId() + ".pdf";

        byte[] pdfByteContent = self.getDocumentData(commonRequestModel);
      CompletableFuture.runAsync(masterDataUtils.withMdc(
            () -> addFilesFromReport(new BASE64DecodedMultipartFile(pdfByteContent), filename,
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
            if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL)) {
                documentType = ReportConstants.ORIGINAL_HAWB;
            } else if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)) {
                documentType = ReportConstants.DRAFT_HAWB;
            }
        }else if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB)){
            documentType = ReportConstants.MAWB;
            if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL)) {
                documentType = ReportConstants.ORIGINAL_MAWB;
            } else if (reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)) {
                documentType = ReportConstants.DRAFT_MAWB;
            }
        }

        return documentType;
    }
    
    public DocumentManagerResponse<DocumentManagerDataResponse> addFilesFromReport(MultipartFile file, String filename, DocUploadRequest uploadRequest, String entityKey) {
        try {
            var shipmentSettings = commonUtils.getShipmentSettingFromContext();
            // If Shipment V3 is disabled
            if (shipmentSettings != null && !Boolean.TRUE.equals(shipmentSettings.getIsRunnerV3Enabled())) {
                var uploadResponse = documentManagerService.temporaryFileUpload(file, filename);
                if (!Boolean.TRUE.equals(uploadResponse.getSuccess()))
                    throw new IOException("File Upload Failed");

                return documentManagerService.saveFile(DocumentManagerSaveFileRequest.builder().fileName(filename)
                        .entityType(uploadRequest.getEntityType())
                        .secureDownloadLink(uploadResponse.getData().getSecureDownloadLink())
                        .fileSize(uploadResponse.getData().getFileSize())
                        .fileType(uploadResponse.getData().getFileType())
                        .path(uploadResponse.getData().getPath())
                        .entityKey(entityKey)
                        .source(Constants.SYSTEM_GENERATED)
                        .docType(uploadRequest.getDocType())
                        .docName(uploadRequest.getDocType())
                        .childType(uploadRequest.getDocType())
                        .isTransferEnabled(uploadRequest.getIsTransferEnabled())
                        .build());
            }
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
        Optional<ShipmentDetails> shipmentDetails;
        if(request.getId() != null ){
            shipmentDetails = shipmentDao.findById(id);
        } else {
            UUID guid = UUID.fromString(request.getGuid());
            shipmentDetails = shipmentDao.findByGuid(guid);
        }
        if (shipmentDetails.isEmpty()) {
            log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Map<String, Object> dataRetrived = new HashMap<>();
        shipmentTagsForExteranlServices.populateRaKcDataWithShipmentDetails(dataRetrived, shipmentDetails.get());
        return ResponseHelper.buildSuccessResponse(dataRetrived);
    }

    private Awb setPrintTypeForAwb(ReportRequest reportRequest, Boolean isOriginalPrint) {
        var originalPrintedAt = LocalDateTime.now();
        Awb awb = null;
        if((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) && Boolean.TRUE.equals(isOriginalPrint)) {
            if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && !reportRequest.isFromShipment())
                awb = awbDao.updateAwbPrintInformation(null, Long.parseLong(reportRequest.getReportId()), PrintType.ORIGINAL_PRINTED, isOriginalPrint, originalPrintedAt);
            else
                awb = awbDao.updateAwbPrintInformation(Long.parseLong(reportRequest.getReportId()), null, PrintType.ORIGINAL_PRINTED, isOriginalPrint, originalPrintedAt);
        } else if ((reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) || reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.HAWB)) && reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.DRAFT)) {
            if(reportRequest.getReportInfo().equalsIgnoreCase(ReportConstants.MAWB) && !reportRequest.isFromShipment())
                awb = awbDao.updateAwbPrintInformation(null, Long.parseLong(reportRequest.getReportId()), PrintType.DRAFT_PRINTED, isOriginalPrint, null);
            else
                awb = awbDao.updateAwbPrintInformation(Long.parseLong(reportRequest.getReportId()), null, PrintType.DRAFT_PRINTED, isOriginalPrint, null);
        }

        return awb;
    }

    public byte[] printForPartiesAndBarcode(ReportRequest reportRequest, List<byte[]> pdfBytes, String number, Map<String, Object> dataRetrived, DocPages pages) throws DocumentException, IOException {
        String[] printingForParties = null;
        if (reportRequest.getPrintingFor_str() != null && reportRequest.getPrintingFor_str().equalsIgnoreCase("0")) {
            printingForParties = new String[]{"1","2","3","4","5","6","7","8","9","10","11","12"};
        } else if (StringUtility.isNotEmpty(reportRequest.getPrintingFor_str())) {
            printingForParties = reportRequest.getPrintingFor_str().split(",");
        }
        List<Future<byte[]>> futures = new ArrayList<>();

        for (String party : printingForParties) {
            futures.add(executorService.submit(() -> {
                Map<String, Object> threadSafeData = new HashMap<>(dataRetrived); // avoid shared mutation
                MawbPrintFor printForParty = MawbPrintFor.getById(Integer.parseInt(party));
                threadSafeData.put(ReportConstants.PRINTING_FOR, printForParty.getDesc());

                byte[] mainDocPage = getFromDocumentService(threadSafeData, pages.getMainPageId());
                if (mainDocPage == null) {
                    throw new ValidationException(ReportConstants.PLEASE_UPLOAD_VALID_TEMPLATE);
                }

                mainDocPage = getMainDocPage(reportRequest, number, printForParty, mainDocPage);

                return mainDocPage;
            }));
        }
        for (Future<byte[]> future : futures) {
            try {
                pdfBytes.add(future.get());
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                throw new GenericException(e.getMessage());
            }
        }
        return CommonUtils.concatAndAddContent(pdfBytes);
    }

    private byte[] getMainDocPage(ReportRequest reportRequest, String number, MawbPrintFor printForParty, byte[] mainDocPage) throws IOException, DocumentException {
        if (Boolean.FALSE.equals(printForParty.getPrintTermsAndCondition())) {
            mainDocPage = CommonUtils.removeLastPage(mainDocPage);
            mainDocPage = CommonUtils.addBlankPage(mainDocPage);
        }

        if (reportRequest.isPrintBarcode()) {
            mainDocPage = addBarCodeInReport(mainDocPage, number, 140, -50, ReportConstants.MAWB, false);
        }
        return mainDocPage;
    }


    public void triggerAutomaticTransfer(IReport report, ReportRequest reportRequest){
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            if (triggerRequirementNotMatching(shipmentSettingsDetails, reportRequest))
                return;
            Long reportId = Long.parseLong(reportRequest.getReportId());
            Boolean isRunnerV3Enabled = shipmentSettingsDetails.getIsRunnerV3Enabled();
            if (report instanceof HblReport) {
                ShipmentDetails shipmentDetails = getShipmentDetails(reportRequest);
                if(shipmentDetails!=null) {
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerBlAutomaticTransfer(shipmentDetails, isRunnerV3Enabled)), executorService);
                }
            } else if (report instanceof HawbReport) {
                ShipmentDetails shipmentDetails = getShipmentDetails(reportRequest);
                if(shipmentDetails!=null) {
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerHAWBAutomaticTransfer(shipmentDetails, isRunnerV3Enabled)), executorService);
                }
            } else if (report instanceof MawbReport) {
                if (!reportRequest.isFromShipment()) { // Case: Request came from consolidation
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerConsoleMAWBAutomaticTransfer(reportId, isRunnerV3Enabled)), executorService);
                } else {
                    ShipmentDetails shipmentDetails = getShipmentDetails(reportRequest);
                    if(shipmentDetails!=null) {
                        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> triggerShipmentMAWBAutomaticTransfer(shipmentDetails, isRunnerV3Enabled)), executorService);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error while triggering automatic transfer for report {}, errorMsg: {}", reportRequest.getReportInfo(), e.getMessage());
        }
    }

    private boolean triggerRequirementNotMatching(ShipmentSettingsDetails shipmentSettingsDetails, ReportRequest reportRequest) {
        if (!Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled())) {
            return true;
        }
        return reportRequest.getPrintType() != null && !reportRequest.getPrintType().equalsIgnoreCase(ReportConstants.ORIGINAL);
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

    public void triggerBlAutomaticTransfer(ShipmentDetails shipmentDetails, Boolean isRunnerV3Enabled){
        if(!CommonUtils.setIsNullOrEmpty(shipmentDetails.getConsolidationList())){
            for(ConsolidationDetails consolidationDetails: shipmentDetails.getConsolidationList()){
                if (consolidationDetails!=null  &&
                        (Objects.equals(Constants.TRANSPORT_MODE_SEA, consolidationDetails.getTransportMode()) &&
                                !Objects.equals(Constants.CONSOLIDATION_TYPE_DRT, consolidationDetails.getConsolidationType()))) {
                    triggerConsoleAutomaticTransfer(isRunnerV3Enabled, consolidationDetails);
                }
            }
        }
    }

    private void triggerConsoleAutomaticTransfer(Boolean isRunnerV3Enabled, ConsolidationDetails consolidationDetails) {
        if(Boolean.TRUE.equals(isRunnerV3Enabled))
            networkTransferV3Util.triggerAutomaticTransfer(consolidationDetails, null, true);
        else
            consolidationService.triggerAutomaticTransfer(consolidationDetails, null, true);
    }

    public void triggerHAWBAutomaticTransfer(ShipmentDetails shipmentDetails, Boolean isRunnerV3Enabled){
        if(ObjectUtils.isNotEmpty(shipmentDetails.getConsolidationList())){
            for(ConsolidationDetails consolidationDetails: shipmentDetails.getConsolidationList()){
                if (consolidationDetails!=null &&
                        (Objects.equals(Constants.TRANSPORT_MODE_AIR, consolidationDetails.getTransportMode()) &&
                                Objects.equals(Constants.SHIPMENT_TYPE_STD, shipmentDetails.getJobType()))) {
                    triggerConsoleAutomaticTransfer(isRunnerV3Enabled, consolidationDetails);
                }
            }
        }
    }

    public void triggerConsoleMAWBAutomaticTransfer(Long consolidationId, Boolean isRunnerV3Enabled){
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consolidationId).orElse(null);
        if(consolidationDetails==null)
            return;
        if(Objects.equals(Constants.TRANSPORT_MODE_AIR, consolidationDetails.getTransportMode()) &&
                Objects.equals(Constants.SHIPMENT_TYPE_STD, consolidationDetails.getConsolidationType())) {
            triggerConsoleAutomaticTransfer(isRunnerV3Enabled, consolidationDetails);
        }
    }

    public void triggerShipmentMAWBAutomaticTransfer(ShipmentDetails shipmentDetails, Boolean isRunnerV3Enabled){
        if(Objects.equals(Constants.TRANSPORT_MODE_AIR, shipmentDetails.getTransportMode()) &&
                Objects.equals(Constants.SHIPMENT_TYPE_DRT, shipmentDetails.getJobType())) {
            if(Boolean.TRUE.equals(isRunnerV3Enabled))
                networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails, null, true);
            else
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
        Set<String> usernamesList = getUsernamesList(shipmentDetails);

        Map<String, String> usernameEmailsMap = new HashMap<>();
        var unlocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.getLocationDataFromCache(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(),
                shipmentDetails.getCarrierDetails().getDestinationPort(),
                shipmentDetails.getCarrierDetails().getOrigin(),
                shipmentDetails.getCarrierDetails().getDestination()).filter(Objects::nonNull).collect(Collectors.toSet()), unLocMap)));
        var templatesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getEmailTemplate(emailTemplateId, emailTemplatesRequests)), executorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> commonUtils.getUserDetails(usernamesList, usernameEmailsMap)), executorService);

        CompletableFuture.allOf(unlocationsFuture, templatesFuture, userEmailsFuture).join();

        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOrigin()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOrigin()))
            map.put(ORIGIN, unLocMap.get(shipmentDetails.getCarrierDetails().getOrigin()).getName());
        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestination()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestination()))
            map.put(DSTN, unLocMap.get(shipmentDetails.getCarrierDetails().getDestination()).getName());
        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort()))
            map.put(POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort()))
            map.put(POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());

        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
            cc.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
            cc.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
    }

    private Set<String> getUsernamesList(ShipmentDetails shipmentDetails) {
        Set<String> usernamesList = new HashSet<>();
        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getCreatedBy()))
            usernamesList.add(shipmentDetails.getCreatedBy());
        if(!CommonUtils.isStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            usernamesList.add(shipmentDetails.getAssignedTo());
        return usernamesList;
    }

    private String getPartyString(Parties parties) {
        if(Objects.isNull(parties))
            return null;
        return String.join(", ", ReportHelper.getOrgAddress(modelMapper.map(parties, PartiesModel.class)));
    }

    private String getContNums(ShipmentDetails shipmentDetails) {
        List<String> response = new ArrayList<>();
        if(!CommonUtils.setIsNullOrEmpty(shipmentDetails.getContainersList())) {
            shipmentDetails.getContainersList().stream().filter(e -> !CommonUtils.isStringNullOrEmpty(e.getContainerNumber())).forEach(e -> response.add(e.getContainerNumber()));
        }
        return String.join(", ", response);
    }

    public void pushFileToDocumentMaster(ReportRequest reportRequest, byte[] pdfByteContent, Map<String, Object> dataRetrieved) {
        log.info("{} | {} Starting pushFileToDocumentMaster process for request {}.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, jsonHelper.convertToJson(reportRequest));
        var shipmentSettings = commonUtils.getShipmentSettingFromContext();
        log.info("{} | {} pushFileToDocumentMaster Shipment Settings Fetched for tenantId: {} --- With Shipments V3 Flag: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, TenantContext.getCurrentTenant(), shipmentSettings != null && Boolean.TRUE.equals(shipmentSettings.getIsRunnerV3Enabled()));
        // If Shipment V3 is enabled && when this method is called for first time, should not push when this method is called internally
        if (shipmentSettings != null  && Boolean.TRUE.equals(shipmentSettings.getIsRunnerV3Enabled()) && Boolean.FALSE.equals(reportRequest.isSelfCall())) {
            log.info("{} | {} Processing pushFileToDocumentMaster process as Shipment3.0Flag enabled.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE);
            String filename;
            String childType;
            String docType = reportRequest.getReportInfo();

            // Generate FileName, childType & DocType based on request Type
            switch (reportRequest.getReportInfo()) {
                case FCR_DOCUMENT:
                    filename = StringUtility.convertToString(dataRetrieved.get(FCR_NO)) + DocumentConstants.DOT_PDF;
                    childType = StringUtility.convertToString(dataRetrieved.get(FCR_NO));
                    break;
                case TRANSPORT_ORDER:
                    filename = StringUtility.convertToString(dataRetrieved.get(REFERENCE_NO))+ DocumentConstants.DOT_PDF;
                    childType = StringUtility.convertToString(dataRetrieved.get(REFERENCE_NO));
                    break;
                case HOUSE_BILL:
                    filename =  HOUSE_BILL + DocumentConstants.DASH + reportRequest.getPrintType() + DocumentConstants.DASH + reportRequest.getReportId() + DocumentConstants.DOT_PDF;
                    childType = reportRequest.getPrintType();
                    docType = DocumentConstants.HBL;
                    break;
                case SEAWAY_BILL:
                    filename = SEAWAY_BILL + DocumentConstants.DASH + reportRequest.getReportId() + DocumentConstants.DOT_PDF;
                    childType = SEA_WAYBILL;
                    docType = DocumentConstants.HBL;
                    break;
                case HAWB, MAWB:
                    filename = reportRequest.getReportInfo() + DocumentConstants.DASH + reportRequest.getPrintType() + DocumentConstants.DASH + reportRequest.getReportId() + DocumentConstants.DOT_PDF;
                    childType = reportRequest.getPrintType();
                    break;
                default:
                    filename = reportRequest.getReportInfo() + DocumentConstants.DASH + reportRequest.getReportId() + DocumentConstants.DOT_PDF;
                    childType = reportRequest.getPrintType();
            }

            try {
                DocUploadRequest docUploadRequest = new DocUploadRequest();
                docUploadRequest.setDocType(docType);
                docUploadRequest.setChildType(childType);
                docUploadRequest.setFileName(filename);
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.setDocumentServiceParameters(reportRequest, docUploadRequest, pdfByteContent)), executorService);
            } catch (Exception e) {
                log.error("{} | {} : {} : Exception: {}", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE,"pushFileToDocumentMaster", e.getMessage());
            }
        } else {
            log.info("{} | {} Ending pushFileToDocumentMaster process for tenantID {} as Shipment3.0Flag disabled.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, TenantContext.getCurrentTenant());
        }
    }

    private void setDocumentServiceParameters(ReportRequest reportRequest,  DocUploadRequest docUploadRequest, byte[] pdfByteContent) {
        String transportMode;
        String shipmentType;
        String consolidationType;
        String entityGuid;
        String entityType;
        log.info("{} | {} Starting setDocumentServiceParameters process for Doc request {}.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, jsonHelper.convertToJson(docUploadRequest));

        // Set TransportMode, ShipmentType, EntityKey, EntityType based on report Module Type
        switch (reportRequest.getEntityName()) {
            case Constants.SHIPMENT:
                ShipmentDetails shipmentDetails = shipmentDao.findById(Long.valueOf(reportRequest.getReportId())).orElse(new ShipmentDetails());
                transportMode = shipmentDetails.getTransportMode();
                shipmentType = shipmentDetails.getDirection();
                consolidationType = shipmentDetails.getJobType();
                entityGuid = StringUtility.convertToString(shipmentDetails.getGuid());
                entityType = Constants.SHIPMENTS_WITH_SQ_BRACKETS;
                break;

            case Constants.CONSOLIDATION:
                ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(Long.valueOf(reportRequest.getReportId())).orElse(new ConsolidationDetails());
                transportMode = consolidationDetails.getTransportMode();
                shipmentType = consolidationDetails.getShipmentType();
                consolidationType = consolidationDetails.getConsolidationType();
                entityGuid = StringUtility.convertToString(consolidationDetails.getGuid());
                entityType = Constants.CONSOLIDATIONS_WITH_SQ_BRACKETS;
                break;

            default:
                log.warn("{} | {} | Invalid Module Type: {}", LoggerHelper.getRequestIdFromMDC(), "setDocumentServiceParameters", reportRequest.getEntityName());
                return;
        }

        docUploadRequest.setEntityType(entityType);
        docUploadRequest.setKey(entityGuid);
        docUploadRequest.setTransportMode(transportMode);
        docUploadRequest.setShipmentType(shipmentType);
        docUploadRequest.setConsolidationType(consolidationType);
        log.info("{} | {} Processing setDocumentServiceParameters process for Doc request {}.... ", LoggerHelper.getRequestIdFromMDC(), LoggerEvent.PUSH_DOCUMENT_TO_DOC_MASTER_VIA_REPORT_SERVICE, jsonHelper.convertToJson(docUploadRequest));
        documentManagerService.pushSystemGeneratedDocumentToDocMaster(new BASE64DecodedMultipartFile(pdfByteContent), docUploadRequest.getFileName(), docUploadRequest);
    }
}
