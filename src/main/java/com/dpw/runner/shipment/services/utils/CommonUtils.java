package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.intraBranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.TimeZoneConstants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.v1.response.CoLoadingMAWBDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.notification.service.impl.NotificationServiceImpl;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.itextpdf.text.*;
import com.itextpdf.text.pdf.*;
import lombok.extern.slf4j.Slf4j;
import net.sourceforge.barbecue.Barcode;
import net.sourceforge.barbecue.BarcodeException;
import net.sourceforge.barbecue.BarcodeFactory;
import net.sourceforge.barbecue.BarcodeImageHandler;
import net.sourceforge.barbecue.output.OutputException;
import org.krysalis.barcode4j.impl.upcean.EAN13Bean;
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionSystemException;

import javax.imageio.ImageIO;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD_CAPS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.MasterDataConstants.ITEM_TYPE;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.*;
import static com.dpw.runner.shipment.services.utils.DateUtils.convertDateToUserTimeZone;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Component
@Slf4j
public class CommonUtils {

    @Autowired
    private NotificationServiceImpl notificationService;
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    public ExecutorService syncExecutorService;

    @Autowired
    public IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private TenantSettingsService tenantSettingsService;

    @Autowired
    private IV1Service iv1Service;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private ICarrierDetailsDao carrierDetailsDao;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Value("${current-base-url}")
    private String baseUrl;

    public static FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

    public static BufferedImage generateEAN13BarcodeImage(String barcodeText, int resolution) {
        EAN13Bean barcodeGenerator = new EAN13Bean();
        BitmapCanvasProvider canvas =
                new BitmapCanvasProvider(resolution, BufferedImage.TYPE_BYTE_BINARY, false, 0);

        barcodeGenerator.generateBarcode(canvas, barcodeText);
        return canvas.getBufferedImage();
    }

    public static byte[] generateBarcodeImage(String barcodeText) throws BarcodeException, OutputException {
        Barcode barcode = BarcodeFactory.createCode128(barcodeText);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        BarcodeImageHandler.writePNG(barcode, outputStream);
        byte[] data = outputStream.toByteArray();
        outputStream.reset();
        return data;
    }

    public static ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator) {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(1);
        request.setPageSize(Integer.MAX_VALUE);


        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }

    public static ListCommonRequest constructListRequestFromEntityId(Long entityId, String entityType) {
        FilterCriteria entityIdCriteria = FilterCriteria.builder()
                .innerFilter(Arrays.asList(FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("entityId")
                                        .operator("=")
                                        .value(entityId)
                                        .build()).build(),
                        FilterCriteria.builder()
                                .logicOperator("AND")
                                .criteria(Criteria.builder()
                                        .fieldName("entityType")
                                        .operator("=")
                                        .value(entityType)
                                        .build())
                                .build()))
                .build();

        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(Integer.MAX_VALUE)
                .filterCriteria(Arrays.asList(entityIdCriteria))
                .build();

        return listCommonRequest;
    }

    public static Criteria getFilterCriteria(String fieldName, Object value, String operator) {
        return Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
    }

    public static ListCommonRequest andCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if(request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        List<FilterCriteria> criterias = request.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if(innerFilters.size() > 0) {
            filterCriteria.setLogicOperator("and");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public static ListCommonRequest orCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if(request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        List<FilterCriteria> criterias = request.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if(innerFilters.size() > 0) {
            filterCriteria.setLogicOperator("or");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public <T,P> P convertToClass(T obj, Class<P> clazz) {
        return jsonHelper.convertValue(obj, clazz);
    }

    public <T,P extends IRunnerResponse > List<P> convertToDtoList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }

    public <T,P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }

    public <T,P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz, Boolean isCreate) {
        return  lst.stream()
                .map(item -> isCreate ? this.convertToCreateClass(item, clazz) : convertToClass(item, clazz))
                .toList();
    }

    public <T,P extends MultiTenancy> List<P> convertToCreateEntityList(final List<T> lst, Class<P> clazz) {
        return  lst.stream()
                .map(item -> this.convertToCreateClass(item, clazz))
                .toList();
    }

    public <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClassModelMapper(item, clazz))
                .toList();
    }

    private  <T,P> P convertToClassModelMapper(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }

    public <T,P> P convertToCreateClass(T obj, Class<P> clazz) {
        return jsonHelper.convertCreateValue(obj, clazz);
    }

    public static byte[] ImageToByte(BufferedImage img) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(img, "jpg", baos);
        byte[] data = baos.toByteArray();
        baos.reset();
        return data;
    }

    public static boolean HasUnsupportedCharacters(String input) {
        int minSupportedAscii = 32;
        int maxSupportedAscii = 126;
        for (char c : input.toCharArray()) {
            if ((int) c < minSupportedAscii || (int) c > maxSupportedAscii) {
                return true;
            }
        }
        return false;
    }

    public static byte[] concatAndAddContent(List<byte[]> pdfByteContent) throws DocumentException, IOException {
        ByteArrayOutputStream ms = new ByteArrayOutputStream();
        Document doc = null;
        PdfCopy copy = null;
        doc = new Document();
        copy = new PdfSmartCopy(doc, ms);
        doc.open();

        for (byte[] dataByte : pdfByteContent) {
            PdfReader reader = null;
            reader = new PdfReader(dataByte);
            copy.addDocument(reader);
            reader.close();
        }
        doc.close();
        copy.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static byte[] removeLastPage(byte[] bytes) throws IOException, DocumentException {
        PdfReader r = new PdfReader(bytes);
        ByteArrayOutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfWriter w = PdfWriter.getInstance(doc, ms);
        doc.open();
        var pagesToKeep = r.getNumberOfPages();
        for(int page=1; page<pagesToKeep; page++){
            doc.newPage();
            w.getDirectContent().addTemplate(w.getImportedPage(r, page), 0, 0);
        }
        w.close();
        r.close();
        doc.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static byte[] getLastPage(byte[] bytes) throws IOException, DocumentException {
        PdfReader r = new PdfReader(bytes);
        ByteArrayOutputStream ms = new ByteArrayOutputStream();
        Document doc = new Document();
        PdfWriter w = PdfWriter.getInstance(doc, ms);
        doc.open();
        doc.newPage();
        w.getDirectContent().addTemplate(w.getImportedPage(r, r.getNumberOfPages()), 0, 0);
        w.close();
        r.close();
        doc.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static void AddWaterMark(PdfContentByte dc, String text, BaseFont font, float fontSize, float angle, BaseColor color, Rectangle realPageSize, Rectangle rect)
    {
        var gstate = new PdfGState();
        gstate.setFillOpacity(0.2f);
        gstate.setStrokeOpacity(0.3f);
        dc.saveState();
        dc.setGState(gstate);
        dc.setColorFill(color);
        dc.beginText();
        dc.setFontAndSize(font, fontSize);
        var ps = rect == null ? realPageSize : rect; /*dc.PdfDocument.PageSize is not always correct*/
        var x = (ps.getRight() + ps.getLeft()) / 2;
        var y = (ps.getBottom() + ps.getTop()) / 2;
        dc.showTextAligned(Element.ALIGN_CENTER, text, x, y, angle);
        dc.endText();
        dc.restoreState();
    }

    public static byte[] addWatermarkToPdfBytes(byte[] bytes, BaseFont bf, String watermark) throws IOException, DocumentException {
        ByteArrayOutputStream ms = new ByteArrayOutputStream(10 * 1024);
        PdfReader reader = new PdfReader(bytes);
        PdfStamper stamper = new PdfStamper(reader, ms);
        int times = reader.getNumberOfPages();
        for (int i = 1; i <= times; i++)
        {
            var dc = stamper.getOverContent(i);
            AddWaterMark(dc, watermark, bf, 50, 35, new BaseColor(70, 70, 255), reader.getPageSizeWithRotation(i), null);
        }
        stamper.close();
        reader.close();
        byte[] data = ms.toByteArray();
        ms.reset();
        return data;
    }

    public static ByteArrayResource getByteResource(InputStream inputStream, String fileName) throws IOException {
        return new ByteArrayResource(inputStream.readAllBytes()) {
            @Override
            public String getFilename() {
                return fileName;
            }
        };
    }

    public static double roundOffToTwoDecimalPlace(double number) {
        DecimalFormat decimalFormat = new DecimalFormat("#.##");
        return Double.parseDouble(decimalFormat.format(number));
    }

    public static String stringValueOf(Object o) {
        if(o == null)
            return null;
        return o.toString();
    }

    public static boolean IsStringNullOrEmpty(String s){
        return s == null || s.isEmpty();
    }

    public static <T> boolean listIsNullOrEmpty(List<T> list) {
        return list == null || list.isEmpty();
    }

    public static Integer getIntFromString(String s) {
        if(IsStringNullOrEmpty(s))
            return null;
        return Integer.parseInt(s);
    }

    public static String getErrorResponseMessage(Exception e, Class<?> clazz) {
        String responseMessage = "";
    responseMessage =
        switch (e.getClass().getSimpleName()) {
          case "TransactionSystemException" -> Objects.requireNonNull(
                  ((TransactionSystemException) e).getRootCause())
              .getMessage();
          default -> e.getMessage();
        };
        return responseMessage;
    }

    public static String getConstrainViolationErrorMessage(Exception e) {
        String errorMessage = "";
        Set<ConstraintViolation<?>> set = ((ConstraintViolationException) e).getConstraintViolations();
        List<String> errors = set.stream().map(i -> String.format("%s : %s",i.getInvalidValue(), i.getMessage())).toList();
        errorMessage = errors.toString();
        return errorMessage;
    }

    public static String inWords(Long num) {
        String[] a = {"", "One ", "Two ", "Three ", "Four ", "Five ", "Six ", "Seven ", "Eight ", "Nine ", "Ten ",
                "Eleven ", "Twelve ", "Thirteen ", "Fourteen ", "Fifteen ", "Sixteen ", "Seventeen ", "Eighteen ",
                "Nineteen "};
        String[] b = {"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"};

        if (num > 999999999) {
            return "overflow";
        }

        String numStr = String.format("%09d", num);
        int[] n = {
                Integer.parseInt(numStr.substring(0, 2)), // Crore
                Integer.parseInt(numStr.substring(2, 4)), // Lakh
                Integer.parseInt(numStr.substring(4, 6)), // Thousand
                Integer.parseInt(numStr.substring(6, 7)), // Hundred
                Integer.parseInt(numStr.substring(7, 9))  // Tens and Ones
        };

        StringBuilder str = new StringBuilder();

        str.append((n[0] != 0) ? getTwoDigitWordConversion(a, b, n, 0) + "Crore " : "");
        str.append((n[1] != 0) ? getTwoDigitWordConversion(a, b, n, 1) + "Lakh " : "");
        str.append((n[2] != 0) ? getTwoDigitWordConversion(a, b, n, 2) + "Thousand " : "");
        str.append((n[3] != 0) ? (!a[n[3]].equals("") ? a[n[3]] : b[n[3] / 10] + " " + a[n[3] % 10]) + "Hundred " : "");
        str.append((n[4] != 0) ? ((str.length() != 0) ? "and " : "") +
                getTwoDigitWordConversion(a, b, n, 4) + " " : "");

        return str.toString().trim();
    }

    private static String getTwoDigitWordConversion(String[] a, String[] b, int[] n, int unitPlaceFromLeft) {
        if(a[n[unitPlaceFromLeft] % 10].equals(""))
            return b[n[unitPlaceFromLeft] / 10] + " ";
        else {
            if(n[unitPlaceFromLeft] / 10 != 0)
                return b[n[unitPlaceFromLeft] / 10] + " " + a[n[unitPlaceFromLeft] % 10];
            else
                return a[n[unitPlaceFromLeft] % 10];
        }
    }

    public static <T> Iterable<T> emptyIfNull(Iterable<T> iterable) {
        return iterable == null ? Collections.emptyList() : iterable;
    }

    public ShipmentSettingsDetails getShipmentSettingFromContext() {
        Optional<ShipmentSettingsDetails> optional = shipmentSettingsDao.getSettingsByTenantIdWithCache(TenantContext.getCurrentTenant());
        return optional.orElseGet(() -> ShipmentSettingsDetails.builder().weightDecimalPlace(2).volumeDecimalPlace(3).build());
    }


    public static boolean areTimeStampsEqual(LocalDateTime a, LocalDateTime b) {
        if(a == null || b == null)
            return false;
        var res = a.truncatedTo(ChronoUnit.MINUTES).compareTo(b.truncatedTo(ChronoUnit.MINUTES));
        return res == 0;
    }

    public V1TenantSettingsResponse getCurrentTenantSettings() {
        return tenantSettingsService.getV1TenantSettings(TenantContext.getCurrentTenant());
    }

    public InterBranchDto getInterBranchContext() {
        return InterBranchContext.getContext();
    }

    public void removeInterBranchContext() {
        InterBranchContext.removeContext();
    }

    public void setInterBranchContextForHub() {
        /**
         * Check current branch should be enabled both
         * Set isHub = true && coloadStationsTenantIds (TenantSettings + Current)
         */
        var tenantSettings = getCurrentTenantSettings();
        var interBranchDto = InterBranchDto.builder().build();

        if (Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())
                && Boolean.TRUE.equals(tenantSettings.getIsColoadingMAWBStationEnabled())
                && !Objects.isNull(tenantSettings.getColoadingBranchIds())) {
            interBranchDto.setColoadStationsTenantIds(tenantSettings.getColoadingBranchIds());
            interBranchDto.setHub(true);
        }

        InterBranchContext.setContext(interBranchDto);
    }

    public void setInterBranchContextForColoadStation() {
        /**
         * Check current branch should be enabled both IsMAWBColoadingEnabled
         * Set isCoLoadStation = true && hubTenantIds (TenantSettings + Current)
         */
        var tenantSettings = getCurrentTenantSettings();
        var interBranchDto = InterBranchDto.builder().hubTenantIds(Arrays.asList()).build();

        if (Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())
            && !Objects.isNull(tenantSettings.getColoadingBranchIds())) {
            interBranchDto.setHubTenantIds(fetchColoadingDetails().stream().map(CoLoadingMAWBDetailsResponse::getParentTenantId).toList());
            interBranchDto.setCoLoadStation(true);
        }

        InterBranchContext.setContext(interBranchDto);
    }

    public List<CoLoadingMAWBDetailsResponse> fetchColoadingDetails() {
        List<Object> criteria = new ArrayList<>(List.of(List.of("ChildTenantId"), "=", TenantContext.getCurrentTenant()));
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(100).criteriaRequests(criteria).build();
        var v1Response = iv1Service.getCoLoadingStations(commonV1ListRequest);
        return jsonHelper.convertValueToList(v1Response.entities, CoLoadingMAWBDetailsResponse.class);
    }

    public ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws RunnerException {
        String responseMsg;
        try {
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if(consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null && consolidationDetails.getAllocations().getWeightUnit() != null) {
                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal weight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                if(Objects.equals(weight, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setWeightUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setWeightUtilization( String.valueOf( (consolidatedWeight.divide(weight, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue() ) );
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                if(Objects.equals(volume, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization( String.valueOf( (consolidatedVolume.divide(volume, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue() ) );
            }
            return consolidationDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public void updateConsolOpenForAttachment(ConsolidationDetails consolidationDetails) {
        if(!Objects.isNull(consolidationDetails.getAchievedQuantities())) {
            Double weightUtilization = consolidationDetails.getAchievedQuantities().getWeightUtilization() != null ? Double.valueOf(consolidationDetails.getAchievedQuantities().getWeightUtilization()) : 0;
            Double volumeUtilization = consolidationDetails.getAchievedQuantities().getVolumeUtilization() != null ? Double.valueOf(consolidationDetails.getAchievedQuantities().getVolumeUtilization()) : 0;
            if(weightUtilization > 100 || volumeUtilization > 100)
                consolidationDetails.setOpenForAttachment(false);
        }
    }

    public void sendEmailForPushRequested(ConsolidationDetails consolidationDetails, ShipmentDetails shipmentDetails, Set<ShipmentRequestedType> shipmentRequestedTypes) {
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new HashMap<>();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        List<EntityTransferMasterLists> toAndCcMailIds = new ArrayList<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        usernamesList.add(shipmentDetails.getCreatedBy());
        usernamesList.add(shipmentDetails.getAssignedTo());
        usernamesList.add(consolidationDetails.getCreatedBy());
        tenantIds.add(consolidationDetails.getTenantId());

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getEmailTemplate(SHIPMENT_PULL_REJECTED, emailTemplatesRequests)), syncExecutorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getCarriersData(Stream.of(shipmentDetails.getCarrierDetails().getShippingLine()).filter(Objects::nonNull).toList(), carrierMasterDataMap)), syncExecutorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getUnLocationsData(Stream.of(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort()).filter(Objects::nonNull).toList(), unLocMap)), syncExecutorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getToAndCCEmailIds(tenantIds, toAndCcMailIds)), syncExecutorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getUserDetails(usernamesList, usernameEmailsMap)), syncExecutorService);
        CompletableFuture.allOf(emailTemplateFuture, carrierFuture, unLocationsFuture, toAndCcEmailIdsFuture, userEmailsFuture);

        Map<Integer, List<EntityTransferMasterLists>> toAndCCMasterDataMap = toAndCcMailIds.stream().collect(Collectors.groupingBy(EntityTransferMasterLists::getTenantId));

        try {
            sendEmailForPullPushRequestStatus(shipmentDetails, consolidationDetails, SHIPMENT_PUSH_REQUESTED, null, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, toAndCCMasterDataMap, null);
        } catch (Exception e) {
            log.error("Error while sending email");
        }
    }

    public void sendEmailForPullPushRequestStatus(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, ShipmentRequestedType type, String rejectRemarks,
                                                  Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequestMap, Set<ShipmentRequestedType> shipmentRequestedTypes, Map<String, UnlocationsResponse> unLocMap,
                                                  Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, String> usernameEmailsMap, Map<Integer, List<EntityTransferMasterLists>> toAndCCMasterDataMap, String requestedUser) throws Exception{
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(type == SHIPMENT_PULL_REQUESTED) {
            if(!emailTemplatesRequestMap.containsKey(SHIPMENT_PULL_REQUESTED)) {
                shipmentRequestedTypes.add(SHIPMENT_PULL_REQUESTED);
                return;
            }
            EmailTemplatesRequest emailTemplatesRequest =  emailTemplatesRequestMap.get(SHIPMENT_PULL_REQUESTED);
            Map<String, Object> dictionary = new HashMap<>();
            populateDictionaryForPullRequested(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);

            if(!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                toEmailIds.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            if(!IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
                toEmailIds.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
            if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
                ccEmailIds.add(UserContext.getUser().getEmail());
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, shipmentDetails.getTenantId(), true);

            notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                    emailTemplatesRequest.getSubject(), toEmailIds.stream().toList(), ccEmailIds.stream().toList());
            return;
        }
        if(type == SHIPMENT_PULL_ACCEPTED) {
            if(!emailTemplatesRequestMap.containsKey(SHIPMENT_PULL_ACCEPTED)) {
                shipmentRequestedTypes.add(SHIPMENT_PULL_ACCEPTED);
                return;
            }
            EmailTemplatesRequest emailTemplatesRequest =  emailTemplatesRequestMap.get(SHIPMENT_PULL_ACCEPTED);
            Map<String, Object> dictionary = new HashMap<>();
            populateDictionaryForPullAccepted(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);

            if(!IsStringNullOrEmpty(consolidationDetails.getCreatedBy()) && usernameEmailsMap.containsKey(consolidationDetails.getCreatedBy()))
                toEmailIds.add(usernameEmailsMap.get(consolidationDetails.getCreatedBy()));
            if(IsStringNullOrEmpty(requestedUser) && usernameEmailsMap.containsKey(requestedUser))
                ccEmailIds.add(usernameEmailsMap.get(requestedUser));
            if(!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                ccEmailIds.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            if(!IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
                ccEmailIds.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
            if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
                ccEmailIds.add(UserContext.getUser().getEmail());
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, consolidationDetails.getTenantId(), false);

            notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                    emailTemplatesRequest.getSubject(), toEmailIds.stream().toList(), ccEmailIds.stream().toList());
            return;
        }
        if(type == SHIPMENT_PULL_REJECTED) {
            if(!emailTemplatesRequestMap.containsKey(SHIPMENT_PULL_REJECTED)) {
                shipmentRequestedTypes.add(SHIPMENT_PULL_REJECTED);
                return;
            }
            EmailTemplatesRequest emailTemplatesRequest =  emailTemplatesRequestMap.get(SHIPMENT_PULL_REJECTED);
            Map<String, Object> dictionary = new HashMap<>();
            populateDictionaryForPullRejected(dictionary, shipmentDetails, consolidationDetails, rejectRemarks);

            if(!IsStringNullOrEmpty(consolidationDetails.getCreatedBy()) && usernameEmailsMap.containsKey(consolidationDetails.getCreatedBy()))
                toEmailIds.add(usernameEmailsMap.get(consolidationDetails.getCreatedBy()));
            if(IsStringNullOrEmpty(requestedUser) && usernameEmailsMap.containsKey(requestedUser))
                ccEmailIds.add(usernameEmailsMap.get(requestedUser));
            if(!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                ccEmailIds.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            if(!IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
                ccEmailIds.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
            if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
                ccEmailIds.add(UserContext.getUser().getEmail());
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, consolidationDetails.getTenantId(), false);

            notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                    emailTemplatesRequest.getSubject(), toEmailIds.stream().toList(), ccEmailIds.stream().toList());
        }
        if(type == SHIPMENT_PUSH_REQUESTED) {
            if(!emailTemplatesRequestMap.containsKey(SHIPMENT_PUSH_REQUESTED)) {
                shipmentRequestedTypes.add(SHIPMENT_PUSH_REQUESTED);
                return;
            }
            EmailTemplatesRequest emailTemplatesRequest =  emailTemplatesRequestMap.get(SHIPMENT_PUSH_REQUESTED);
            Map<String, Object> dictionary = new HashMap<>();
            populateDictionaryForPushRequested(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);

            if(!IsStringNullOrEmpty(consolidationDetails.getCreatedBy()) && usernameEmailsMap.containsKey(consolidationDetails.getCreatedBy()))
                toEmailIds.add(usernameEmailsMap.get(consolidationDetails.getCreatedBy()));
            if(!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                ccEmailIds.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            if(!IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
                ccEmailIds.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
            if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
                ccEmailIds.add(UserContext.getUser().getEmail());
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, consolidationDetails.getTenantId(), false);

            notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                    emailTemplatesRequest.getSubject(), toEmailIds.stream().toList(), ccEmailIds.stream().toList());
            return;
        }
        if(type == SHIPMENT_PUSH_ACCEPTED) {
            if(!emailTemplatesRequestMap.containsKey(SHIPMENT_PUSH_ACCEPTED)) {
                shipmentRequestedTypes.add(SHIPMENT_PUSH_ACCEPTED);
                return;
            }
            EmailTemplatesRequest emailTemplatesRequest = emailTemplatesRequestMap.get(SHIPMENT_PUSH_ACCEPTED);
            Map<String, Object> dictionary = new HashMap<>();
            populateDictionaryForPushAccepted(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);

            if(!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                toEmailIds.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            if(!IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
                toEmailIds.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
            if(IsStringNullOrEmpty(requestedUser) && usernameEmailsMap.containsKey(requestedUser))
                ccEmailIds.add(usernameEmailsMap.get(requestedUser));
            if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
                ccEmailIds.add(UserContext.getUser().getEmail());
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, shipmentDetails.getTenantId(), true);

            notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                    emailTemplatesRequest.getSubject(), toEmailIds.stream().toList(), ccEmailIds.stream().toList());
            return;
        }
        if(type == SHIPMENT_PUSH_REJECTED) {
            if(!emailTemplatesRequestMap.containsKey(SHIPMENT_PUSH_REJECTED)) {
                shipmentRequestedTypes.add(SHIPMENT_PUSH_REJECTED);
                return;
            }
            EmailTemplatesRequest emailTemplatesRequest = emailTemplatesRequestMap.get(SHIPMENT_PUSH_REJECTED);
            Map<String, Object> dictionary = new HashMap<>();
            populateDictionaryForPushRejected(dictionary, shipmentDetails, consolidationDetails, rejectRemarks);

            if(!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()) && usernameEmailsMap.containsKey(shipmentDetails.getAssignedTo()))
                toEmailIds.add(usernameEmailsMap.get(shipmentDetails.getAssignedTo()));
            if(!IsStringNullOrEmpty(shipmentDetails.getCreatedBy()) && usernameEmailsMap.containsKey(shipmentDetails.getCreatedBy()))
                toEmailIds.add(usernameEmailsMap.get(shipmentDetails.getCreatedBy()));
            if(IsStringNullOrEmpty(requestedUser) && usernameEmailsMap.containsKey(requestedUser))
                ccEmailIds.add(usernameEmailsMap.get(requestedUser));
            if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
                ccEmailIds.add(UserContext.getUser().getEmail());
            // fetching to and cc from master lists
            getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, toAndCCMasterDataMap, shipmentDetails.getTenantId(), true);

            notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                    emailTemplatesRequest.getSubject(), toEmailIds.stream().toList(), ccEmailIds.stream().toList());
        }
    }

    private void getToAndCcEmailMasterLists(Set<String> toEmailIds, Set<String> ccEmailIds, Map<Integer, List<EntityTransferMasterLists>> toAndCCMasterDataMap,
                                            Integer tenantId, boolean isShipment) {
        int toId = MasterDataType.ConsolidationAttachDefaultToMailId.getId();
        int ccId = MasterDataType.ConsolidationAttachDefaultCCMailId.getId();
        if(isShipment) {
            toId = MasterDataType.ShipmentAttachDefaultToMailId.getId();
            ccId = MasterDataType.ShipmentAttachDefaultCCMailId.getId();
        }
        if(toAndCCMasterDataMap.containsKey(tenantId)) {
            for(EntityTransferMasterLists entityTransferMasterLists : toAndCCMasterDataMap.get(tenantId)) {
                if(!IsStringNullOrEmpty(entityTransferMasterLists.getItemValue())) {
                    if(entityTransferMasterLists.getItemType() == toId)
                        toEmailIds.addAll(Arrays.stream(entityTransferMasterLists.getItemValue().split(",")).map(String::trim)
                                .filter(s -> !s.isEmpty()).toList());

                    if(entityTransferMasterLists.getItemType() == ccId)
                        ccEmailIds.addAll(Arrays.stream(entityTransferMasterLists.getItemValue().split(",")).map(String::trim)
                                .filter(s -> !s.isEmpty()).toList());
                }
            }
        }
    }

    public void populateDictionaryForPullRequested(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                   Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(Consol_Branch_Code, UserContext.getUser().getCode());
        dictionary.put(Consol_Branch_Name, UserContext.getUser().getTenantDisplayName());
        dictionary.put(User_name, consolidationDetails.getCreatedBy());
    }

    public void populateDictionaryForPullAccepted(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromShipment(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(Actioned_User_name, shipmentDetails.getCreatedBy());
    }

    public void populateDictionaryForPullRejected(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String rejectRemarks) {
        dictionary.put(Consolidation_Create_User, consolidationDetails.getCreatedBy());
        dictionary.put(Shipment_Branch_Code, UserContext.getUser().getCode());
        dictionary.put(Shipment_Branch_Name, UserContext.getUser().getTenantDisplayName());
        dictionary.put(Interbranch_Consolidation_Number, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(Reject_remarks, rejectRemarks);
        dictionary.put(Actioned_User_name, shipmentDetails.getCreatedBy());
    }

    public void populateDictionaryForPushRequested(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromShipment(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(User_name, shipmentDetails.getCreatedBy());
    }

    public void populateDictionaryForPushAccepted(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                   Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(Actioned_User_name, shipmentDetails.getCreatedBy());
    }

    public void populateDictionaryForPushRejected(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String rejectRemarks) {
        dictionary.put(Shipment_Create_User, shipmentDetails.getCreatedBy());
        dictionary.put(Shipment_Assigned_User, shipmentDetails.getAssignedTo());
        dictionary.put(Interbranch_Shipment_Number, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(Source_Consolidation_Number, consolidationDetails.getConsolidationNumber());
        dictionary.put(Reject_remarks, rejectRemarks);
        dictionary.put(Actioned_User_name, shipmentDetails.getCreatedBy());
    }

    public void populateDictionaryForEmailFromShipment(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        dictionary.put(Consolidation_Create_User, consolidationDetails.getCreatedBy());
        dictionary.put(Shipment_Branch_Code, UserContext.getUser().getCode());
        dictionary.put(Shipment_Branch_Name, UserContext.getUser().getTenantDisplayName());
        dictionary.put(Interbranch_Consolidation_Number, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(Shipment_Number, shipmentDetails.getShipmentId());
        dictionary.put(HAWB_Number, shipmentDetails.getHouseBill());
        dictionary.put(ETD_CAPS, shipmentDetails.getCarrierDetails().getEtd());
        dictionary.put(ETA_CAPS, shipmentDetails.getCarrierDetails().getEta());
//        dictionary.put(Cargo_Receive_Date, new field);
        if(!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getShippingLine())) {
            dictionary.put(Carrier_Code, shipmentDetails.getCarrierDetails().getShippingLine());
            if(carrierMasterDataMap.containsKey(shipmentDetails.getCarrierDetails().getShippingLine()))
                dictionary.put(Carrier_name, carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(Flight_Number, shipmentDetails.getCarrierDetails().getFlightNumber());
        if(!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_Name, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if(!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_Name, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(Shipment_Weight, shipmentDetails.getWeight());
        dictionary.put(Shipment_Weight_Unit, shipmentDetails.getWeightUnit());
        dictionary.put(Shipment_Volume, shipmentDetails.getVolume());
        dictionary.put(Shipment_Volume_Unit, shipmentDetails.getVolumeUnit());
        dictionary.put(Request_Date_Time, convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false));
    }

    public void populateDictionaryForEmailFromConsolidation(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                       Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        dictionary.put(Shipment_Create_User, shipmentDetails.getCreatedBy());
        dictionary.put(Shipment_Assigned_User, shipmentDetails.getAssignedTo());
        dictionary.put(Interbranch_Shipment_Number, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(Consolidation_Number, consolidationDetails.getConsolidationNumber());
        dictionary.put(Source_Consolidation_Number, consolidationDetails.getConsolidationNumber());
        dictionary.put(MAWB_Number, consolidationDetails.getMawb());
        dictionary.put(ETD_CAPS, consolidationDetails.getCarrierDetails().getEtd());
        dictionary.put(ETA_CAPS, consolidationDetails.getCarrierDetails().getEta());
        // LAT to be added
        if(!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine())) {
            dictionary.put(Carrier_Code, consolidationDetails.getCarrierDetails().getShippingLine());
            if(carrierMasterDataMap.containsKey(consolidationDetails.getCarrierDetails().getShippingLine()))
                dictionary.put(Carrier_name, carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(Flight_Number, consolidationDetails.getCarrierDetails().getFlightNumber());
        if(!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_Name, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if(!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_Name, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(Allocated_Weight, consolidationDetails.getAllocations().getWeight());
        dictionary.put(Allocated_Weight_Unit, consolidationDetails.getAllocations().getWeightUnit());
        dictionary.put(Allocated_Volume, consolidationDetails.getAllocations().getVolume());
        dictionary.put(Allocated_Volume_Unit, consolidationDetails.getAllocations().getVolumeUnit());
        dictionary.put(Request_Date_Time, convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false));
    }

    public void getUnLocationsData(List<String> unLocGuids, Map<String, UnlocationsResponse> map) {
        if(unLocGuids == null || unLocGuids.isEmpty())
            return;
        Map<String, UnlocationsResponse> tempMap = masterDataUtils.getLocationData(new HashSet<>(unLocGuids));
        map.putAll(tempMap);
    }

    public void getCarriersData(List<String> carrierCodes, Map<String, CarrierMasterData> map) {
        if(carrierCodes == null || carrierCodes.isEmpty())
            return;
        Map<String, CarrierMasterData> tempMap = masterDataUtils.getCarriersData(new HashSet<>(carrierCodes));
        map.putAll(tempMap);
    }

    public String getShipmentIdHyperLink(String shipmentId, Long Id) {
        String link = baseUrl + "v2/shipments/edit/" + Id;
        return "<html><body>" + "<p>" + shipmentId + "</p>" + "<a href='" + link + "'>Click here</a>" + "</body></html>"; // send proper link here
    }

    public String getConsolidationIdHyperLink(String consolidationId, Long Id) {
        String link = baseUrl + "v2/shipments/consolidations/edit/" + Id;
        return "<html><body>" + "<p>" + consolidationId + "</p>" + "<a href='" + link + "'>Click here</a>" + "</body></html>"; // send proper link here
    }

    private String replaceTagsFromData(Map<String, Object> map, String val) {
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if(!Objects.isNull(entry.getValue()) && !Objects.isNull(entry.getKey()))
                val = val.replace(entry.getKey(), entry.getValue().toString());
        }
        return val;
    }

    public void getEmailTemplate(ShipmentRequestedType type, Map<ShipmentRequestedType, EmailTemplatesRequest> response) {
        List<String> requests = new ArrayList<>();
        if(type == SHIPMENT_PULL_REQUESTED)
            requests.add("Attach Shipment Request");
        if(type == SHIPMENT_PULL_ACCEPTED) {
            requests.add("Consolidation Request - Accept");
            requests.add("Shipment Request Reject");
            requests.add("Consolidation Request - Rejected");
        }
        if(type == SHIPMENT_PULL_REJECTED)
            requests.add("Consolidation Request - Rejected");
        if(type == SHIPMENT_PUSH_REQUESTED)
            requests.add("Attach Consolidation Request");
        if(type == SHIPMENT_PUSH_ACCEPTED) {
            requests.add("Shipment Request Accept");
            requests.add("Shipment Request Reject");
            requests.add("Consolidation Request - Rejected");
        }
        if(type == SHIPMENT_PUSH_REJECTED)
            requests.add("Shipment Request Reject");
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        if(v1DataResponse != null)
        {
            List<EmailTemplatesRequest> emailTemplatesRequests = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
            if(emailTemplatesRequests != null && !emailTemplatesRequests.isEmpty()) {
                for (EmailTemplatesRequest emailTemplatesRequest : emailTemplatesRequests) {
                    if(Objects.equals(emailTemplatesRequest.getType(), "Attach Shipment Request"))
                        response.put(SHIPMENT_PULL_REQUESTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), "Consolidation Request - Accept"))
                        response.put(SHIPMENT_PULL_ACCEPTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), "Consolidation Request - Rejected"))
                        response.put(SHIPMENT_PULL_REJECTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), "Attach Consolidation Request"))
                        response.put(SHIPMENT_PUSH_REQUESTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), "Shipment Request Accept"))
                        response.put(SHIPMENT_PUSH_ACCEPTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), "Shipment Request Reject"))
                        response.put(SHIPMENT_PUSH_REJECTED, emailTemplatesRequest);
                }
            }
        }
    }

    public void getToAndCCEmailIds(Set<Integer> tenantIds, List<EntityTransferMasterLists> toAndCcMailIds) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        request.setTake(tenantIds.size() * 4);

        List<Object> field = new ArrayList<>(List.of("TenantId"));
        String operator = Operators.IN.getValue();
        List<Object> criteria1 = new ArrayList<>(List.of(field, operator, List.of(tenantIds.stream().toList())));

        field = new ArrayList<>(List.of(ITEM_TYPE));
        List<Object> criteria2 = new ArrayList<>(List.of(field, operator, List.of(List.of(MasterDataType.ConsolidationAttachDefaultToMailId.getId(),
                MasterDataType.ConsolidationAttachDefaultCCMailId.getId(), MasterDataType.ShipmentAttachDefaultToMailId.getId(),
                MasterDataType.ShipmentAttachDefaultCCMailId.getId()))));

        request.setCriteriaRequests(new ArrayList<>(List.of(criteria1, "and", criteria2)));
        V1DataResponse v1DataResponse = iv1Service.getMasterDetails(request);
        toAndCcMailIds.addAll(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class));
    }

    public void getUserDetails(Set<String> usernamesList, Map<String, String> usernameEmailsMap) {
        usernamesList.remove(null);
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of("Username"));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(usernamesList)));
        request.setCriteriaRequests(criteria);
        request.setTake(usernamesList.size());
        V1DataResponse v1DataResponse = iv1Service.getUserDetails(request);
        List<UsersDto> usersDtos = jsonHelper.convertValueToList(v1DataResponse.entities, UsersDto.class);
        usernameEmailsMap.putAll(usersDtos.stream().collect(Collectors.toMap(UsersDto::getUsername, UsersDto::getEmail)));
    }

    public void updateUnLocData(CarrierDetails carrierDetails, CarrierDetails oldCarrierDetails) {
        try {
            if( !Objects.isNull(carrierDetails) && ( Objects.isNull(oldCarrierDetails) || !Objects.equals(carrierDetails.getOrigin(), oldCarrierDetails.getOrigin())
                    || !Objects.equals(carrierDetails.getOriginPort(), oldCarrierDetails.getOriginPort())
                    || !Objects.equals(carrierDetails.getDestination(), oldCarrierDetails.getDestination())
                    || !Objects.equals(carrierDetails.getDestinationPort(), oldCarrierDetails.getDestinationPort()) )) {
                List<String> unlocoRequests = new ArrayList<>();
                if(!IsStringNullOrEmpty(carrierDetails.getOrigin()))
                    unlocoRequests.add(carrierDetails.getOrigin());
                if(!IsStringNullOrEmpty(carrierDetails.getOriginPort()))
                    unlocoRequests.add(carrierDetails.getOriginPort());
                if(!IsStringNullOrEmpty(carrierDetails.getDestination()))
                    unlocoRequests.add(carrierDetails.getDestination());
                if(!IsStringNullOrEmpty(carrierDetails.getDestinationPort()))
                    unlocoRequests.add(carrierDetails.getDestinationPort());
                Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
                UnlocationsResponse pol = unlocationsMap.get(carrierDetails.getOriginPort());
                UnlocationsResponse pod = unlocationsMap.get(carrierDetails.getDestinationPort());
                UnlocationsResponse origin = unlocationsMap.get(carrierDetails.getOrigin());
                UnlocationsResponse destination = unlocationsMap.get(carrierDetails.getDestination());
                carrierDetails.setOriginLocCode(origin.getLocCode());
                carrierDetails.setDestinationLocCode(destination.getLocCode());
                carrierDetails.setOriginPortLocCode(pol.getLocCode());
                carrierDetails.setDestinationPortLocCode(pod.getLocCode());
                carrierDetailsDao.saveUnLocCodes(carrierDetails);
            }
        }
        catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierDetails.getId(), e.getMessage());
        }
    }

}