package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchTenantIdContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.TimeZoneConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.SendEmailDto;
import com.dpw.runner.shipment.services.dto.v1.request.DGTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TenantDetailsByListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1RoleIdRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
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
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.commons.constants.CacheConstants.CARRIER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DESTINATION_PORT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HAWB_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MAWB_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ORIGIN_PORT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOYAGE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.*;
import static com.dpw.runner.shipment.services.utils.DateUtils.convertDateToUserTimeZone;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Component
@Slf4j
public class CommonUtils {
    private final ICarrierDetailsDao carrierDetailsDao;
    private final INotificationService notificationService;

    @Autowired
    public CommonUtils(ICarrierDetailsDao carrierDetailsDao, INotificationService notificationService) {
        this.carrierDetailsDao = carrierDetailsDao;
        this.notificationService = notificationService;
    }

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    public ExecutorService syncExecutorService;

    @Autowired
    public IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IAuditLogDao iAuditLogDao;

    @Autowired
    private TenantSettingsService tenantSettingsService;

    @Autowired
    private IV1Service iv1Service;



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
        if(criterias.isEmpty()) {
            criterias.add(FilterCriteria.builder().innerFilter(new ArrayList<>()).build());
        }
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

    public static Integer getCurrentWorkingTenantId() {
        if(InterBranchTenantIdContext.getContext() != null)
            return InterBranchTenantIdContext.getContext().getTenantId();
        return UserContext.getUser().TenantId;
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
            if(Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR)
                    && (weightUtilization > 100 || volumeUtilization > 100))
                consolidationDetails.setOpenForAttachment(false);
        }
    }

    private void fetchDataForRejectionExplicitEmails(List<ShipmentDetails> shipmentDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                                     Set<Integer> tenantIds, Set<String> usernamesList, List<ConsolidationDetails> otherConsolidationDetails,
                                                     Map<String, String> usernameEmailsMap, Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests,
                                                     Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap) {
        for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
        }

        for(ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        for(ConsolidationDetails consolidationDetails1 : otherConsolidationDetails) {
            usernamesList.add(consolidationDetails1.getCreatedBy());
            tenantIds.add(consolidationDetails1.getTenantId());
        }

        var emailTemplateFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getEmailTemplate(emailTemplatesRequests)), syncExecutorService);
        var toAndCcEmailIdsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getToAndCCEmailIdsFromTenantSettings(tenantIds, v1TenantSettingsMap)), syncExecutorService);
        var userEmailsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> getUserDetails(usernamesList, usernameEmailsMap)), syncExecutorService);
        CompletableFuture.allOf(emailTemplateFuture, toAndCcEmailIdsFuture, userEmailsFuture).join();
    }

    public void sendRejectionEmailsExplicitly(List<ShipmentDetails> shipmentDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                              Set<ShipmentRequestedType> shipmentRequestedTypes, List<ConsolidationDetails> otherConsolidationDetails) {
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests =  new EnumMap<>(ShipmentRequestedType.class);
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        // fetch data from db and v1
        fetchDataForRejectionExplicitEmails(shipmentDetails, consoleShipmentMappings, tenantIds, usernamesList, otherConsolidationDetails, usernameEmailsMap, emailTemplatesRequests, v1TenantSettingsMap);

        if(!otherConsolidationDetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationDetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            Map<Long, ShipmentDetails> finalShipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e1 -> e1));
            consoleShipmentMappings.forEach(consoleShipmentMapping -> {
                try {
                    if(finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId()) && finalShipmentDetailsMap.containsKey(consoleShipmentMapping.getShipmentId())) {
                        if(consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy());
                        else
                            sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy());
                    }
                } catch (Exception e) {
                    log.error(ERROR_WHILE_SENDING_EMAIL);
                }
            });
        }
    }

    public void sendEmailShipmentPullRequest(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_REQUESTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_REQUESTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest =  sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_REQUESTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullRequested(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap());

        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            ccEmailIds.add(UserContext.getUser().getEmail());
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailResponseToDGRequester(Map<OceanDGStatus, EmailTemplatesRequest> emailTemplates,
        OceanDGRequest request, OceanDGStatus status, ShipmentDetails shipmentDetails) throws RunnerException {
        EmailTemplatesRequest template = Optional.ofNullable(emailTemplates.get(status))
            .orElseThrow(() -> new RunnerException("No template is present for status: " + status));

        Map<String, Object> dictionary = new HashMap<>();
        List<String> recipientEmails = Collections.singletonList(request.getRequesterUserEmailId());

        populateDGReceiverDictionary(dictionary, shipmentDetails);

        notificationService.sendEmail(replaceTagsFromData(dictionary, template.getBody()),
            template.getSubject(), new ArrayList<>(recipientEmails), null);
    }
    public void sendEmailShipmentPullAccept(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_ACCEPTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_ACCEPTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest =  sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_ACCEPTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullAccepted(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap(), sendEmailDto.getRequestedUser());

        if(!IsStringNullOrEmpty(sendEmailDto.getConsolidationDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getConsolidationDetails().getCreatedBy()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getConsolidationDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(sendEmailDto.getRequestedUser()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getRequestedUser()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getRequestedUser()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            ccEmailIds.add(UserContext.getUser().getEmail());
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPullReject(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_REJECTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_REJECTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest =  sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_REJECTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullRejected(dictionary, sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getRequestedUser());

        if(!IsStringNullOrEmpty(sendEmailDto.getConsolidationDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getConsolidationDetails().getCreatedBy()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getConsolidationDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(sendEmailDto.getRequestedUser()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getRequestedUser()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getRequestedUser()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            ccEmailIds.add(UserContext.getUser().getEmail());
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushRequest(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_REQUESTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_REQUESTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest =  sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_REQUESTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushRequested(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap());

        if(!IsStringNullOrEmpty(sendEmailDto.getConsolidationDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getConsolidationDetails().getCreatedBy()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getConsolidationDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            ccEmailIds.add(UserContext.getUser().getEmail());
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), false);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushAccept(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_ACCEPTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_ACCEPTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_ACCEPTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushAccepted(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap(), sendEmailDto.getRequestedUser());

        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(sendEmailDto.getRequestedUser()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getRequestedUser()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getRequestedUser()));
        if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            ccEmailIds.add(UserContext.getUser().getEmail());
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), false);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushReject(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if(!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_REJECTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_REJECTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_REJECTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushRejected(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getRequestedUser());

        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if(!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            toEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
        if(!IsStringNullOrEmpty(sendEmailDto.getRequestedUser()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getRequestedUser()))
            ccEmailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getRequestedUser()));
        if(!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            ccEmailIds.add(UserContext.getUser().getEmail());
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), false);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailForPullPushRequestStatus(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, ShipmentRequestedType type, String rejectRemarks,
                                                  Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequestMap, Set<ShipmentRequestedType> shipmentRequestedTypes, Map<String, UnlocationsResponse> unLocMap,
                                                  Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, String> usernameEmailsMap, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap , String requestedUser) throws Exception{
        SendEmailDto sendEmailDto = SendEmailDto.builder()
                .shipmentDetails(shipmentDetails)
                .consolidationDetails(consolidationDetails)
                .type(type)
                .rejectRemarks(rejectRemarks)
                .emailTemplatesRequestMap(emailTemplatesRequestMap)
                .shipmentRequestedTypes(shipmentRequestedTypes)
                .unLocMap(unLocMap)
                .carrierMasterDataMap(carrierMasterDataMap)
                .usernameEmailsMap(usernameEmailsMap)
                .v1TenantSettingsMap(v1TenantSettingsMap)
                .requestedUser(requestedUser)
                .build();
        sendEmailForPullPushRequestStatus(sendEmailDto);
    }

    public void sendEmailForPullPushRequestStatus(SendEmailDto sendEmailDto) throws Exception{
        switch (sendEmailDto.getType()) {
            case SHIPMENT_PULL_REQUESTED -> sendEmailShipmentPullRequest(sendEmailDto);
            case SHIPMENT_PULL_ACCEPTED -> sendEmailShipmentPullAccept(sendEmailDto);
            case SHIPMENT_PULL_REJECTED -> sendEmailShipmentPullReject(sendEmailDto);
            case SHIPMENT_PUSH_REQUESTED -> sendEmailShipmentPushRequest(sendEmailDto);
            case SHIPMENT_PUSH_ACCEPTED -> sendEmailShipmentPushAccept(sendEmailDto);
            case SHIPMENT_PUSH_REJECTED -> sendEmailShipmentPushReject(sendEmailDto);
        }
    }

    public void getToAndCcEmailMasterLists(Set<String> toEmailIds, Set<String> ccEmailIds, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap, Integer tenantId, boolean isShipment) {
        if(v1TenantSettingsMap.containsKey(tenantId)) {
            if(isShipment) {
                if(!IsStringNullOrEmpty(v1TenantSettingsMap.get(tenantId).getShipmentAttachDefaultToMailId()))
                    toEmailIds.addAll(Arrays.stream(v1TenantSettingsMap.get(tenantId).getShipmentAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
                if(!IsStringNullOrEmpty(v1TenantSettingsMap.get(tenantId).getShipmentAttachDefaultCCMailId()))
                    ccEmailIds.addAll(Arrays.stream(v1TenantSettingsMap.get(tenantId).getShipmentAttachDefaultCCMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            } else {
                if(!IsStringNullOrEmpty(v1TenantSettingsMap.get(tenantId).getConsolidationAttachDefaultToMailId()))
                    toEmailIds.addAll(Arrays.stream(v1TenantSettingsMap.get(tenantId).getConsolidationAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
                if(!IsStringNullOrEmpty(v1TenantSettingsMap.get(tenantId).getConsolidationAttachDefaultCCMailId()))
                    ccEmailIds.addAll(Arrays.stream(v1TenantSettingsMap.get(tenantId).getConsolidationAttachDefaultCCMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            }
        }
    }

    public void populateDictionaryForPullRequested(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                   Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(CONSOL_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(CONSOL_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_NAME, consolidationDetails.getCreatedBy());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForPullAccepted(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap, String requestedUser) {
        populateDictionaryForEmailFromShipment(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestedUser);
    }

    public void populateDictionaryForPullRejected(Map<String, Object> dictionary, ConsolidationDetails consolidationDetails, String rejectRemarks, String requestedUser) {
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(SHIPMENT_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(SHIPMENT_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.REJECT_REMARKS, rejectRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestedUser);
    }

    public void populateDictionaryForPushRequested(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        populateDictionaryForEmailFromShipment(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(USER_NAME, shipmentDetails.getCreatedBy());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForPushAccepted(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                   Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap, String requestedUser) {
        populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails, consolidationDetails, unLocMap, carrierMasterDataMap);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestedUser);
    }

    public void populateDictionaryForPushRejected(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String rejectRemarks, String requestUser) {
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.REJECT_REMARKS, rejectRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestUser);
    }

    public void populateDictionaryForEmailFromShipment(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                  Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(SHIPMENT_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(SHIPMENT_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(HAWB_NUMBER, shipmentDetails.getHouseBill());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if(!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(shipmentDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if(IsStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, shipmentDetails.getCarrierDetails().getFlightNumber());
        if(!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if(!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(SHIPMENT_WEIGHT, shipmentDetails.getWeight());
        dictionary.put(SHIPMENT_WEIGHT_UNIT, shipmentDetails.getWeightUnit());
        dictionary.put(SHIPMENT_VOLUME, shipmentDetails.getVolume());
        dictionary.put(SHIPMENT_VOLUME_UNIT, shipmentDetails.getVolumeUnit());
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
    }

    public void populateDictionaryForEmailFromConsolidation(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                                       Map<String, UnlocationsResponse> unLocMap, Map<String, CarrierMasterData> carrierMasterDataMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(MAWB_NUMBER, consolidationDetails.getMawb());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        dictionary.put(LAT, consolidationDetails.getLatDate());
        if(!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(consolidationDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if(IsStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, consolidationDetails.getCarrierDetails().getFlightNumber());
        if(!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if(!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(ALLOCATED_WEIGHT, consolidationDetails.getAllocations().getWeight());
        dictionary.put(ALLOCATED_WEIGHT_UNIT, consolidationDetails.getAllocations().getWeightUnit());
        dictionary.put(ALLOCATED_VOLUME, consolidationDetails.getAllocations().getVolume());
        dictionary.put(ALLOCATED_VOLUME_UNIT, consolidationDetails.getAllocations().getVolumeUnit());
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
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

    public String getShipmentIdHyperLink(String shipmentId, Long id) {
        String link = baseUrl + "/v2/shipments/edit/" + id;
        return HTML_HREF_TAG_PREFIX + link + "'>" + shipmentId + HTML_HREF_TAG_SUFFIX;
    }

    public String getTaskIdHyperLink(String shipmentId, String taskId) {
        String link = baseUrl + "/v2/shipments/tasks/" + taskId;
        return HTML_HREF_TAG_PREFIX + link + "'>" + shipmentId + HTML_HREF_TAG_SUFFIX;
    }

    public String getConsolidationIdHyperLink(String consolidationId, Long id) {
        String link = baseUrl + "/v2/shipments/consolidations/edit/" + id;
        return HTML_HREF_TAG_PREFIX + link + "'>" + consolidationId + HTML_HREF_TAG_SUFFIX;
    }

    public String replaceTagsFromData(Map<String, Object> map, String val) {
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if(!Objects.isNull(entry.getValue()) && !Objects.isNull(entry.getKey()))
                val = val.replace("{" + entry.getKey() + "}", entry.getValue().toString());
        }
        val = val.replaceAll("\\{.*?\\}", "");
        return val;
    }

    public void getEmailTemplate(Map<ShipmentRequestedType, EmailTemplatesRequest> response) {
        List<String> requests = new ArrayList<>(List.of(SHIPMENT_PULL_REQUESTED_EMAIL_TYPE, SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE, SHIPMENT_PUSH_REJECTED_EMAIL_TYPE, SHIPMENT_PULL_REJECTED_EMAIL_TYPE,
                SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE, SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE));
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
                    if(Objects.equals(emailTemplatesRequest.getType(), SHIPMENT_PULL_REQUESTED_EMAIL_TYPE))
                        response.put(SHIPMENT_PULL_REQUESTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE))
                        response.put(SHIPMENT_PULL_ACCEPTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), SHIPMENT_PULL_REJECTED_EMAIL_TYPE))
                        response.put(SHIPMENT_PULL_REJECTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE))
                        response.put(SHIPMENT_PUSH_REQUESTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE))
                        response.put(SHIPMENT_PUSH_ACCEPTED, emailTemplatesRequest);
                    if(Objects.equals(emailTemplatesRequest.getType(), SHIPMENT_PUSH_REJECTED_EMAIL_TYPE))
                        response.put(SHIPMENT_PUSH_REJECTED, emailTemplatesRequest);
                }
            }
        }
    }

    public void getToAndCCEmailIdsFromTenantSettings(Set<Integer> tenantIds, Map<Integer, V1TenantSettingsResponse> tenantSettingsMap) {
        Map<Integer, Object> map = getTenantSettings(new ArrayList<>(tenantIds));
        map.forEach((key, value) -> tenantSettingsMap.put(key, modelMapper.map(value, V1TenantSettingsResponse.class)));
    }

    public Map<Integer, Object> getTenantSettings(List<Integer> tenantIds) {
        if (tenantIds.isEmpty())
            return new HashMap<>();

        try {
            var v1Response = iv1Service.getTenantDetails(TenantDetailsByListRequest.builder().tenantIds(tenantIds).take(100).build());
            return v1Response.getEntities()
                    .stream()
                    .collect(Collectors.groupingBy(
                            TenantDetailsByListResponse.TenantDetails::getTenantId,
                            Collectors.collectingAndThen(
                                    Collectors.toList(),
                                    list -> list.get(0).getTenantSettings()
                            )));
        }
        catch (Exception ex) {
            log.error(ex.getMessage());
            return new HashMap<>();
        }
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

    // called when new dg pack is added or dg pack fields are changed or new dg container is added, or new pack added in dg container or dg container fields are changed
    public boolean changeShipmentDGStatusToReqd(ShipmentDetails shipmentDetails) {
        OceanDGStatus oldOceanDGStatus = shipmentDetails.getOceanDGStatus();
        if(OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) && !UserContext.isOceanDgUser()) {
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);
        }
        if(OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED.equals(shipmentDetails.getOceanDGStatus()) && !UserContext.isOceanDgUser()) {
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);
        }
        if(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentDetails.getOceanDGStatus())) {
            if(!UserContext.isOceanDgCommercialUser())
                shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED);
            if(!UserContext.isOceanDgUser())
                shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);
        }
        if(OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED.equals(shipmentDetails.getOceanDGStatus()) && !UserContext.isOceanDgUser()) {
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);
        }
        return !Objects.equals(oldOceanDGStatus, shipmentDetails.getOceanDGStatus());
    }

    public boolean checkIfDGClass1(String dgClass) {
        return !IsStringNullOrEmpty(dgClass) && dgClass.charAt(0) == '1';
    }

    public boolean checkIfAnyDGClass(String dgClass) throws RunnerException {
        if(!IsStringNullOrEmpty(dgClass)) {
            if(dgClass.charAt(0) == '7')
                throw new RunnerException("As per the DG SOP, you are not allowed to deal in Class 7 DG shipments");
            return true;
        }
        return false;
    }

    public boolean checkIfDGFieldsChangedInPacking(PackingRequest newPack, Packing oldPack) {
        if(!oldPack.getHazardous().equals(newPack.getHazardous()))
            return true;
        if(!Objects.equals(newPack.getDGClass(), oldPack.getDGClass()))
            return true;
        if(!Objects.equals(newPack.getUnNumber(), oldPack.getUnNumber()))
            return true;
        if(!Objects.equals(newPack.getProperShippingName(), oldPack.getProperShippingName()))
            return true;
        if(!Objects.equals(newPack.getPackingGroup(), oldPack.getPackingGroup()))
            return true;
        if(!Objects.equals(newPack.getMinimumFlashPoint(), oldPack.getMinimumFlashPoint()))
            return true;
        if(!Objects.equals(newPack.getMinimumFlashPointUnit(), oldPack.getMinimumFlashPointUnit()))
            return true;
        return !oldPack.getMarinePollutant().equals(newPack.getMarinePollutant());
    }

    public boolean checkIfDGFieldsChangedInContainer(ContainerRequest newContainer, Containers oldContainer) {
        if(!oldContainer.getHazardous().equals(newContainer.getHazardous()))
            return true;
        if(!Objects.equals(newContainer.getDgClass(), oldContainer.getDgClass()))
            return true;
        if(!Objects.equals(newContainer.getUnNumber(), oldContainer.getUnNumber()))
            return true;
        if(!Objects.equals(newContainer.getProperShippingName(), oldContainer.getProperShippingName()))
            return true;
        if(!Objects.equals(newContainer.getPackingGroup(), oldContainer.getPackingGroup()))
            return true;
        if(!Objects.equals(newContainer.getMinimumFlashPoint(), oldContainer.getMinimumFlashPoint()))
            return true;
        if(!Objects.equals(newContainer.getMinimumFlashPointUnit(), oldContainer.getMinimumFlashPointUnit()))
            return true;
        return !oldContainer.getMarinePollutant().equals(newContainer.getMarinePollutant());
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
            }
        }
        catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierDetails.getId(), e.getMessage());
        }
    }

    public String convertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat)
    {
        String strDate = "";
        if (date != null)
        {
            if(!IsStringNullOrEmpty(tsDatetimeFormat))
                strDate = date.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = date.format(getDPWDateFormatOrDefault());
        }
        return strDate;
    }

    public DateTimeFormatter getDPWDateFormatOrDefault()
    {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if(!CommonUtils.IsStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            return DateTimeFormatter.ofPattern(v1TenantSettingsResponse.getDPWDateFormat());
        return DateTimeFormatter.ofPattern("MM/dd/yyyy");
    }

    public static double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    public void populateDictionaryForOceanDGApproval(Map<String,Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks,
        TaskCreateResponse taskCreateResponse){

        populateDictionaryForDGEmailFromShipment(dictionary, shipmentDetails, vesselsResponse, taskCreateResponse);
        populateDictionaryApprovalRequestForDGEmail(dictionary, remarks);
    }

    public void populateDictionaryForOceanDGCommercialApproval(Map<String,Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks, TaskCreateResponse taskCreateResponse){
        populateDictionaryForOceanDGApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);
        List<AuditLog> auditLogList = iAuditLogDao.findByOperationAndEntityId(
            DBOperationType.DG_APPROVE.name(), shipmentDetails.getId());
        if(auditLogList != null){
            Map<String, AuditLogChanges> changesMap = auditLogList.get(0).getChanges();
            OceanDGRequestLog oceanDGRequestLog = mapAuditChangesToOceanDGRequestLog(changesMap);
            dictionary.put(DG_APPROVER_NAME, oceanDGRequestLog.getUserName());
            dictionary.put(DG_APPROVER_TIME, oceanDGRequestLog.getTime());
        }
    }

    public void getDGEmailTemplate(Map<OceanDGStatus, EmailTemplatesRequest> response) {
        List<String> requests = new ArrayList<>(
            List.of(OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE, OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE, OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE,
                OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE, OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE,
                OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE));

        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        if (v1DataResponse != null && v1DataResponse.entities != null) {
            List<EmailTemplatesRequest> emailTemplates = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);

            if (emailTemplates != null && !emailTemplates.isEmpty()) {
                emailTemplates.stream()
                    .filter(Objects::nonNull)
                    .forEach(template -> {
                        switch (template.getType()) {
                            case OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE:
                                response.put(OCEAN_DG_REQUESTED, template);
                                break;
                            case OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE:
                                response.put(OCEAN_DG_ACCEPTED, template);
                                break;
                            case OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE:
                                response.put(OCEAN_DG_REJECTED, template);
                                break;
                            case OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE:
                                response.put(OCEAN_DG_COMMERCIAL_REQUESTED, template);
                                break;
                            case OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE:
                                response.put(OCEAN_DG_COMMERCIAL_ACCEPTED, template);
                                break;
                            case OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE:
                                response.put(OCEAN_DG_COMMERCIAL_REJECTED, template);
                                break;
                            default:
                                break;
                        }
                    });
            }
        }
    }

    public Integer getRoleId(OceanDGStatus oceanDGStatus){
        String roleName = oceanDGStatus == OCEAN_DG_REQUESTED ? OCEAN_DG_ROLE : COMMERCIAL_OCEAN_DG_ROLE;
        return getRoleIDByRoleName(roleName, UserContext.getUser().getTenantId());
    }

    private Integer getRoleIDByRoleName(String roleName, Integer tenantId){
        V1RoleIdRequest v1RoleIdRequest = V1RoleIdRequest
            .builder()
            .roleName(roleName)
            .tenantId(tenantId)
            .build();
        return iv1Service.getRoleIdsByRoleName(v1RoleIdRequest);
    }

    public List<String> getUserEmailsByRoleId(List<String> userEmailIds, Integer roleId) {
        V1UsersEmailRequest request = new V1UsersEmailRequest();
        request.setRoleId(roleId);
        request.setTake(10);
        List<UsersRoleListResponse> userEmailResponse = iv1Service.getUserEmailsByRoleId(request);
        userEmailResponse.forEach(e -> userEmailIds.add(e.getEmail()));

        return userEmailIds;
    }

    public TaskCreateResponse createTask(ShipmentDetails shipmentDetails, Integer roleId, TaskCreateResponse taskCreateResponse)
        throws RunnerException {
        DGTaskCreateRequest taskRequest = DGTaskCreateRequest
            .builder()
            .entityType(Shipments)
            .entityId(shipmentDetails.getId().toString())
            .roleId(roleId.toString())
            .taskType(OCEAN_DG_TASKTYPE)
            .taskStatus(PENDING_ACTION)
            .userId(UserContext.getUser().getUserId())
            .tenantId(UserContext.getUser().getTenantId().toString())
            .build();

        try {
            taskCreateResponse = iv1Service.createTask(taskRequest);
        } catch (Exception e) {
            throw new RunnerException(String.format("Task creation failed for shipmentId: %s. Error: %s",
                shipmentDetails.getId(), e.getMessage()));
        }
        return taskCreateResponse;
    }

    public void getVesselsData(CarrierDetails carrierDetails, VesselsResponse vesselsResponse) {
        if(carrierDetails == null) return;
        String guid = carrierDetails.getVessel();
        if (IsStringNullOrEmpty(guid)) {
            return ;
        }
        List<Object> vesselCriteria = Arrays.asList(
            List.of(Constants.VESSEL_GUID_V1),
            "=",
            guid
        );
        CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(vesselCriteria).build();
        V1DataResponse vesselResponse = iv1Service.fetchVesselData(vesselRequest);
        List<VesselsResponse> vesselsResponseList = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);

        if(vesselsResponseList != null && !vesselsResponseList.isEmpty()) {
          vesselsResponse.setName(vesselsResponseList.get(0).getName());
        }

    }

    private void populateDictionaryForDGEmailFromShipment(Map<String,Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, TaskCreateResponse taskCreateResponse){
        if(shipmentDetails.getCarrierDetails() != null){
            dictionary.put(ORIGIN_PORT, shipmentDetails.getCarrierDetails().getOriginPort());
            dictionary.put(DESTINATION_PORT, shipmentDetails.getCarrierDetails().getDestinationPort());
            dictionary.put(CARRIER, shipmentDetails.getCarrierDetails().getShippingLine());
            dictionary.put(VOYAGE, shipmentDetails.getCarrierDetails().getVoyage());
            dictionary.put(ETA, shipmentDetails.getCarrierDetails().getEta());
            dictionary.put(ETD, shipmentDetails.getCarrierDetails().getEtd());
        }
        dictionary.put(TRANSPORT_MODE, shipmentDetails.getTransportMode());
        dictionary.put(SHIPMENT_TYPE, shipmentDetails.getDirection());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(CARGO_TYPE, shipmentDetails.getShipmentType());
        if(vesselsResponse != null){
            dictionary.put(VESSEL_NAME, vesselsResponse.getName());
        }


        //Summary of cargo Details
        long totalContainerCount = shipmentDetails.getContainersList().stream()
            .mapToLong(Containers::getContainerCount)
            .sum();

        long dgContainerCount = shipmentDetails.getContainersList().stream()
            .filter(Containers::getHazardous)
            .mapToLong(Containers::getContainerCount)
            .sum();

        dictionary.put(CONTAINER_COUNT, totalContainerCount);
        dictionary.put(DG_CONTAINER_COUNT, dgContainerCount);


        String dgPackageTypeAndCount = shipmentDetails.getPackingList().stream()
            .filter(Packing::getHazardous)
            .map(packing -> packing.getPacks() + " " + packing.getPacksType())
            .collect(Collectors.joining(", "));

        String packagesTypeAndCount = shipmentDetails.getPackingList().stream()
            .map(packing -> packing.getPacks() + " " + packing.getPacksType())
            .collect(Collectors.joining(", "));

        dictionary.put(DG_PACKAGES_TYPE, dgPackageTypeAndCount);
        dictionary.put(TOTAL_PACKAGES_TYPE, packagesTypeAndCount);
        dictionary.put(VIEWS, getTaskIdHyperLink(shipmentDetails.getShipmentId(), taskCreateResponse.getTasksId()));
    }

    private void populateDictionaryApprovalRequestForDGEmail(Map<String,Object> dictionary, String remarks) {
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUEST_DATE_TIME, LocalDateTime.now());
        dictionary.put(REQUESTER_REMARKS, remarks);
    }

    private void populateDGReceiverDictionary(Map<String, Object> dictionary, ShipmentDetails shipmentDetails){
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(APPROVER_NAME, UserContext.getUser().getUsername());
        dictionary.put(APPROVED_TIME, LocalDateTime.now());

    }

    private OceanDGRequestLog mapAuditChangesToOceanDGRequestLog(Map<String, AuditLogChanges> changesMap) {
        OceanDGRequestLog log = new OceanDGRequestLog();

        for (AuditLogChanges change : changesMap.values()) {
            switch (change.getFieldName()) {
                case "time":
                    if (change.getNewValue() != null) {
                        log.setTime((LocalDateTime) change.getNewValue());
                    }
                    break;
                case "username":
                    if (change.getNewValue() != null) {
                        log.setUserName((String) change.getNewValue());
                    }
                    break;
                default:
                    break;
            }
        }
        return log;
    }

}