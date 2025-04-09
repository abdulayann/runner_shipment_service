package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.SendEmailDto;
import com.dpw.runner.shipment.services.dto.v1.request.DGTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TenantDetailsByListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TenantFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1RoleIdRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1UsersEmailRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Element;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.BaseFont;
import com.itextpdf.text.pdf.PdfContentByte;
import com.itextpdf.text.pdf.PdfCopy;
import com.itextpdf.text.pdf.PdfGState;
import com.itextpdf.text.pdf.PdfImportedPage;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.PdfSmartCopy;
import com.itextpdf.text.pdf.PdfStamper;
import com.itextpdf.text.pdf.PdfWriter;
import lombok.extern.slf4j.Slf4j;
import net.sourceforge.barbecue.Barcode;
import net.sourceforge.barbecue.BarcodeException;
import net.sourceforge.barbecue.BarcodeFactory;
import net.sourceforge.barbecue.BarcodeImageHandler;
import net.sourceforge.barbecue.output.OutputException;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.Nullable;
import org.krysalis.barcode4j.impl.upcean.EAN13Bean;
import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionSystemException;
import org.springframework.util.CollectionUtils;

import javax.imageio.ImageIO;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.commons.constants.CacheConstants.CARRIER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARRIER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DESTINATION_PORT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.HAWB_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MAWB_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.ORIGIN_PORT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_NUMBER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.STATUS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_NAME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOYAGE;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_DETACH;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PULL_WITHDRAW;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_ACCEPTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REJECTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.SHIPMENT_PUSH_WITHDRAW;
import static com.dpw.runner.shipment.services.utils.CountryListHelper.ISO3166.getAlpha3FromAlpha2;
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

    @Autowired
    IShipmentDao shipmentDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    IMDMServiceAdapter mdmServiceAdapter;

    private static final Map<String, ShipmentRequestedType> EMAIL_TYPE_MAPPING = new HashMap<>();

    static {
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_REQUESTED_EMAIL_TYPE, SHIPMENT_PULL_REQUESTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE, SHIPMENT_PULL_ACCEPTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_REJECTED_EMAIL_TYPE, SHIPMENT_PULL_REJECTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE, SHIPMENT_PUSH_REQUESTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE, SHIPMENT_PUSH_ACCEPTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_REJECTED_EMAIL_TYPE, SHIPMENT_PUSH_REJECTED);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_DETACH_EMAIL_TYPE, SHIPMENT_DETACH);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PULL_WITHDRAW_EMAIL_TYPE, SHIPMENT_PULL_WITHDRAW);
        EMAIL_TYPE_MAPPING.put(SHIPMENT_PUSH_WITHDRAW_EMAIL_TYPE, SHIPMENT_PUSH_WITHDRAW);
    }

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
        if (request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        if (request.getFilterCriteria() == null)
            request.setFilterCriteria(new ArrayList<>());

        List<FilterCriteria> criterias = request.getFilterCriteria();
        if (criterias.isEmpty()) {
            criterias.add(FilterCriteria.builder().innerFilter(new ArrayList<>()).build());
        }
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if (!innerFilters.isEmpty()) {
            filterCriteria.setLogicOperator("and");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public static ListCommonRequest orCriteria(String fieldName, Object value, String operator, ListCommonRequest request) {
        if (request == null) {
            request = new ListCommonRequest();
            request.setPageNo(1);
            request.setPageSize(Integer.MAX_VALUE);
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }

        List<FilterCriteria> criterias = request.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        if (!innerFilters.isEmpty()) {
            filterCriteria.setLogicOperator("or");
        }
        innerFilters.add(filterCriteria);
        return request;
    }

    public <T, P> P convertToClass(T obj, Class<P> clazz) {
        return jsonHelper.convertValue(obj, clazz);
    }

    public <T, P extends IRunnerResponse> List<P> convertToDtoList(final List<T> lst, Class<P> clazz) {
        return lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }

    public <T, P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz) {
        return lst.stream()
                .map(item -> convertToClass(item, clazz))
                .toList();
    }

    public <T, P extends MultiTenancy> List<P> convertToEntityList(final List<T> lst, Class<P> clazz, Boolean isCreate) {
        return lst.stream()
                .map(item -> isCreate ? this.convertToCreateClass(item, clazz) : convertToClass(item, clazz))
                .toList();
    }

    public <T, P extends MultiTenancy> List<P> convertToCreateEntityList(final List<T> lst, Class<P> clazz) {
        return lst.stream()
                .map(item -> this.convertToCreateClass(item, clazz))
                .toList();
    }

    public <T, P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if (lst == null)
            return null;
        return lst.stream()
                .map(item -> convertToClassModelMapper(item, clazz))
                .toList();
    }

    private <T, P> P convertToClassModelMapper(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }

    public <T, P> P convertToCreateClass(T obj, Class<P> clazz) {
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
        for (int page = 1; page < pagesToKeep; page++) {
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

    public static byte[] addBlankPage(byte[] originalPdfBytes) throws IOException, DocumentException {
        // Prepare a ByteArrayOutputStream to hold the modified PDF
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        // Read the original PDF
        PdfReader reader = new PdfReader(originalPdfBytes);
        int numberOfPages = reader.getNumberOfPages();

        // Create a new Document for the output
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, outputStream);

        // Open the new document for writing
        document.open();

        // Copy all pages from the original PDF into the new document
        for (int i = 1; i <= numberOfPages; i++) {
            document.newPage();
            PdfImportedPage page = writer.getImportedPage(reader, i);
            writer.getDirectContent().addTemplate(page, 0, 0);
        }

        // Add a blank page
        document.newPage();
        writer.setPageEmpty(false);

        // Close the resources
        document.close();
        reader.close();

        // Return the modified PDF as a byte array
        return outputStream.toByteArray();
    }

    public static void addWaterMark(PdfContentByte dc, String text, BaseFont font, float fontSize, float angle, BaseColor color, Rectangle realPageSize, Rectangle rect) {
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
        for (int i = 1; i <= times; i++) {
            var dc = stamper.getOverContent(i);
            addWaterMark(dc, watermark, bf, 50, 35, new BaseColor(70, 70, 255), reader.getPageSizeWithRotation(i), null);
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
        if (o == null)
            return null;
        return o.toString();
    }

    public static boolean IsStringNullOrEmpty(String s) {
        return s == null || s.isEmpty();
    }

    public static <T> boolean listIsNullOrEmpty(List<T> list) {
        return list == null || list.isEmpty();
    }

    public static <T> boolean setIsNullOrEmpty(Set<T> set) {
        return set == null || set.isEmpty();
    }

    public static Integer getIntFromString(String s) {
        if (IsStringNullOrEmpty(s))
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
        List<String> errors = set.stream().map(i -> String.format("%s : %s", i.getInvalidValue(), i.getMessage())).toList();
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
        if (a[n[unitPlaceFromLeft] % 10].equals(""))
            return b[n[unitPlaceFromLeft] / 10] + " ";
        else {
            if (n[unitPlaceFromLeft] / 10 != 0)
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
        if (a == null || b == null)
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
            if (consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if (consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null && consolidationDetails.getAllocations().getWeightUnit() != null) {
                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal weight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                if (Objects.equals(weight, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setWeightUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setWeightUtilization(String.valueOf((consolidatedWeight.divide(weight, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue()));
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                if (Objects.equals(volume, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization(String.valueOf((consolidatedVolume.divide(volume, 4, RoundingMode.HALF_UP)).multiply(new BigDecimal(100)).doubleValue()));
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
        if (!Objects.isNull(consolidationDetails.getAchievedQuantities())) {
            Double weightUtilization = consolidationDetails.getAchievedQuantities().getWeightUtilization() != null ? Double.valueOf(consolidationDetails.getAchievedQuantities().getWeightUtilization()) : 0;
            Double volumeUtilization = consolidationDetails.getAchievedQuantities().getVolumeUtilization() != null ? Double.valueOf(consolidationDetails.getAchievedQuantities().getVolumeUtilization()) : 0;
            if (Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR)
                    && (weightUtilization > 100 || volumeUtilization > 100))
                consolidationDetails.setOpenForAttachment(false);
        }
    }

    private void fetchDataForRejectionExplicitEmails(List<ShipmentDetails> shipmentDetails, List<ConsoleShipmentMapping> consoleShipmentMappings,
                                                     Set<Integer> tenantIds, Set<String> usernamesList, List<ConsolidationDetails> otherConsolidationDetails,
                                                     Map<String, String> usernameEmailsMap, Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests,
                                                     Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap) {
        for (ShipmentDetails shipmentDetails1 : shipmentDetails) {
            usernamesList.add(shipmentDetails1.getCreatedBy());
            usernamesList.add(shipmentDetails1.getAssignedTo());
            tenantIds.add(shipmentDetails1.getTenantId());
        }

        for (ConsoleShipmentMapping consoleShipmentMapping : consoleShipmentMappings) {
            usernamesList.add(consoleShipmentMapping.getCreatedBy());
        }
        for (ConsolidationDetails consolidationDetails1 : otherConsolidationDetails) {
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
        Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequests = new EnumMap<>(ShipmentRequestedType.class);
        Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap = new HashMap<>();
        Set<Integer> tenantIds = new HashSet<>();
        Set<String> usernamesList = new HashSet<>();

        // fetch data from db and v1
        fetchDataForRejectionExplicitEmails(shipmentDetails, consoleShipmentMappings, tenantIds, usernamesList, otherConsolidationDetails, usernameEmailsMap, emailTemplatesRequests, v1TenantSettingsMap);

        if (!otherConsolidationDetails.isEmpty()) {
            Map<Long, ConsolidationDetails> finalConsolidationDetailsMap = otherConsolidationDetails.stream().collect(Collectors.toMap(BaseEntity::getId, y -> y));
            Map<Long, ShipmentDetails> finalShipmentDetailsMap = shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, e1 -> e1));
            consoleShipmentMappings.forEach(consoleShipmentMapping -> {
                try {
                    if (finalConsolidationDetailsMap.containsKey(consoleShipmentMapping.getConsolidationId()) && finalShipmentDetailsMap.containsKey(consoleShipmentMapping.getShipmentId())) {
                        if (consoleShipmentMapping.getRequestedType() == SHIPMENT_PUSH_REQUESTED)
                            sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PUSH_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
                        else
                            sendEmailForPullPushRequestStatus(finalShipmentDetailsMap.get(consoleShipmentMapping.getShipmentId()), finalConsolidationDetailsMap.get(consoleShipmentMapping.getConsolidationId()), SHIPMENT_PULL_REJECTED, AUTO_REJECTION_REMARK, emailTemplatesRequests, shipmentRequestedTypes, unLocMap, carrierMasterDataMap, usernameEmailsMap, v1TenantSettingsMap, consoleShipmentMapping.getCreatedBy(), null);
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
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_REQUESTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_REQUESTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_REQUESTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullRequested(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailResponseToDGRequester(EmailTemplatesRequest template,
                                               OceanDGRequest request, ShipmentDetails shipmentDetails) throws RunnerException {


        Map<String, Object> dictionary = new HashMap<>();
        List<String> recipientEmails = Collections.singletonList(request.getUserEmail());

        populateDGReceiverDictionary(dictionary, shipmentDetails, request);


        notificationService.sendEmail(replaceTagsFromData(dictionary, template.getBody()),
                template.getSubject(), new ArrayList<>(recipientEmails), new ArrayList<>());
    }

    public void sendEmailShipmentPullAccept(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_ACCEPTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_ACCEPTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_ACCEPTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullAccepted(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap(), sendEmailDto.getRequestedUser());

        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void populateShipmentImportPullAttachmentTemplate(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, UnlocationsResponse> unLocMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(CONSOL_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(CONSOL_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(MAWB_NUMBER, consolidationDetails.getMawb());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if (!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(consolidationDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (IsStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, consolidationDetails.getCarrierDetails().getFlightNumber());
        if (!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
        dictionary.put(USER_NAME, consolidationDetails.getCreatedBy());
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
    }

    public void populateShipmentImportPushAttachmentTemplate(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, UnlocationsResponse> unLocMap) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(CONSOL_BRANCH_CODE, UserContext.getUser().getCode());
        dictionary.put(CONSOL_BRANCH_NAME, UserContext.getUser().getTenantDisplayName());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(HAWB_NUMBER, shipmentDetails.getHouseBill());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(shipmentDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        if (!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(shipmentDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (IsStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, shipmentDetails.getCarrierDetails().getFlightNumber());
        if (!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
            dictionary.put(ReportConstants.POD, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getLocCode());
            dictionary.put(POD_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getDestinationPort()).getName());
        }
        dictionary.put(BRANCH_TIME_ZONE, MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME));
        dictionary.put(REQUEST_DATE_TIME, convertToDPWDateFormat(convertDateToUserTimeZone(LocalDateTime.now(), MDC.get(TimeZoneConstants.BROWSER_TIME_ZONE_NAME), null, false), tsDateTimeFormat));
        dictionary.put(USER_NAME, shipmentDetails.getCreatedBy());
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void sendEmailNotification(Map<String, Object> dictionary, EmailTemplatesRequest emailTemplateModel, List<String> to, List<String> cc) {
        if (!to.isEmpty()) {
            try {
                notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplateModel.getBody()),
                        replaceTagsFromData(dictionary, emailTemplateModel.getSubject()), to, cc);
            } catch (Exception ex) {
                log.error(ex.getMessage());
            }
        }
    }

    public List<EmailTemplatesRequest> getEmailTemplates(String templateType) {
        List<String> requests = new ArrayList<>(List.of(templateType));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(requests)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        return jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
    }

    public void sendEmailShipmentPullReject(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_REJECTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_REJECTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_REJECTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPullRejected(dictionary, sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getRequestedUser());

        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushRequest(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_REQUESTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_REQUESTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_REQUESTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushRequested(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap());

        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getConsolidationDetails().getTenantId(), false);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushAccept(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_ACCEPTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_ACCEPTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_ACCEPTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushAccepted(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getUnLocMap(), sendEmailDto.getCarrierMasterDataMap(), sendEmailDto.getRequestedUser());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), false);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushReject(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_REJECTED)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_REJECTED);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_REJECTED);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForPushRejected(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getRequestedUser());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), false);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentDetach(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_DETACH)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_DETACH);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_DETACH);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForShipmentDetach(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        getToAndCcEmailMasterLists(toEmailIds, ccEmailIds, sendEmailDto.getV1TenantSettingsMap(), sendEmailDto.getShipmentDetails().getTenantId(), true);

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPullWithdraw(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PULL_WITHDRAW)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PULL_WITHDRAW);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PULL_WITHDRAW);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForShipmentWithdraw(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getTenantModelMap());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, toEmailIds);
        setConsolidationCreatedUserEmail(sendEmailDto, ccEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getShipmentDetails().getTenantId())) {
            if (!IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            if (!IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
        }
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getConsolidationDetails().getTenantId()) &&
                !IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getConsolidationAttachDefaultCCMailId())) {
            ccEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap()
                            .get(sendEmailDto.getConsolidationDetails().getTenantId())
                            .getConsolidationAttachDefaultCCMailId()
                            .split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .toList());
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailShipmentPushWithdraw(SendEmailDto sendEmailDto) {
        Set<String> toEmailIds = new HashSet<>();
        Set<String> ccEmailIds = new HashSet<>();
        if (!sendEmailDto.getEmailTemplatesRequestMap().containsKey(SHIPMENT_PUSH_WITHDRAW)) {
            sendEmailDto.getShipmentRequestedTypes().add(SHIPMENT_PUSH_WITHDRAW);
            return;
        }
        EmailTemplatesRequest emailTemplatesRequest = sendEmailDto.getEmailTemplatesRequestMap().get(SHIPMENT_PUSH_WITHDRAW);
        Map<String, Object> dictionary = new HashMap<>();
        populateDictionaryForConsolidationWithdraw(dictionary, sendEmailDto.getShipmentDetails(), sendEmailDto.getConsolidationDetails(), sendEmailDto.getRejectRemarks(), sendEmailDto.getTenantModelMap());

        setShipmentCreateAndAssignedUserEmail(sendEmailDto, ccEmailIds);
        setConsolidationCreatedUserEmail(sendEmailDto, toEmailIds);
        setRequestedUserEmail(sendEmailDto, toEmailIds);
        setCurrentUserEmail(ccEmailIds);
        // fetching to and cc from master lists
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getConsolidationDetails().getTenantId())) {
            if (!IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getConsolidationAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getConsolidationAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            if (!IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getShipmentAttachDefaultToMailId()))
                toEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getConsolidationDetails().getTenantId()).getShipmentAttachDefaultToMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
        }
        if (sendEmailDto.getV1TenantSettingsMap().containsKey(sendEmailDto.getShipmentDetails().getTenantId())) {
            if (!IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultCCMailId())) {
                ccEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getConsolidationAttachDefaultCCMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            }
            if (!IsStringNullOrEmpty(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultCCMailId())) {
                ccEmailIds.addAll(Arrays.stream(sendEmailDto.getV1TenantSettingsMap().get(sendEmailDto.getShipmentDetails().getTenantId()).getShipmentAttachDefaultCCMailId().split(",")).map(String::trim)
                        .filter(s -> !s.isEmpty()).toList());
            }
        }

        notificationService.sendEmail(replaceTagsFromData(dictionary, emailTemplatesRequest.getBody()),
                replaceTagsFromData(dictionary, emailTemplatesRequest.getSubject()), new ArrayList<>(toEmailIds), new ArrayList<>(ccEmailIds));
    }

    public void sendEmailForPullPushRequestStatus(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, ShipmentRequestedType type, String rejectRemarks,
                                                  Map<ShipmentRequestedType, EmailTemplatesRequest> emailTemplatesRequestMap, Set<ShipmentRequestedType> shipmentRequestedTypes, Map<String, UnlocationsResponse> unLocMap,
                                                  Map<String, CarrierMasterData> carrierMasterDataMap, Map<String, String> usernameEmailsMap, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap,
                                                  String requestedUser, Map<Integer, TenantModel> tenantModelMap) throws Exception {
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
                .tenantModelMap(tenantModelMap)
                .build();
        sendEmailForPullPushRequestStatus(sendEmailDto);
    }

    public void sendEmailForPullPushRequestStatus(SendEmailDto sendEmailDto) throws Exception {
        switch (sendEmailDto.getType()) {
            case SHIPMENT_PULL_REQUESTED -> sendEmailShipmentPullRequest(sendEmailDto);
            case SHIPMENT_PULL_ACCEPTED -> sendEmailShipmentPullAccept(sendEmailDto);
            case SHIPMENT_PULL_REJECTED -> sendEmailShipmentPullReject(sendEmailDto);
            case SHIPMENT_PUSH_REQUESTED -> sendEmailShipmentPushRequest(sendEmailDto);
            case SHIPMENT_PUSH_ACCEPTED -> sendEmailShipmentPushAccept(sendEmailDto);
            case SHIPMENT_PUSH_REJECTED -> sendEmailShipmentPushReject(sendEmailDto);
            case SHIPMENT_DETACH -> sendEmailShipmentDetach(sendEmailDto);
            case SHIPMENT_PULL_WITHDRAW -> sendEmailShipmentPullWithdraw(sendEmailDto);
            case SHIPMENT_PUSH_WITHDRAW -> sendEmailShipmentPushWithdraw(sendEmailDto);
            default -> log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, sendEmailDto.getType());
        }
    }

    public void setShipmentCreateAndAssignedUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getAssignedTo()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getAssignedTo()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getAssignedTo()));
        if (!IsStringNullOrEmpty(sendEmailDto.getShipmentDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getShipmentDetails().getCreatedBy()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getShipmentDetails().getCreatedBy()));
    }

    public void setConsolidationCreatedUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!IsStringNullOrEmpty(sendEmailDto.getConsolidationDetails().getCreatedBy()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getConsolidationDetails().getCreatedBy()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getConsolidationDetails().getCreatedBy()));
    }

    public void setRequestedUserEmail(SendEmailDto sendEmailDto, Set<String> emailIds) {
        if (!IsStringNullOrEmpty(sendEmailDto.getRequestedUser()) && sendEmailDto.getUsernameEmailsMap().containsKey(sendEmailDto.getRequestedUser()))
            emailIds.add(sendEmailDto.getUsernameEmailsMap().get(sendEmailDto.getRequestedUser()));
    }

    public void setCurrentUserEmail(Set<String> emailIds) {
        if (!IsStringNullOrEmpty(UserContext.getUser().getEmail()))
            emailIds.add(UserContext.getUser().getEmail());
    }

    public void getToAndCcEmailMasterLists(Set<String> toEmailIds, Set<String> ccEmailIds, Map<Integer, V1TenantSettingsResponse> v1TenantSettingsMap, Integer tenantId, boolean isShipment) {
        if (v1TenantSettingsMap.containsKey(tenantId)) {
            V1TenantSettingsResponse settings = v1TenantSettingsMap.get(tenantId);
            if (isShipment) {
                addEmails(toEmailIds, settings.getShipmentAttachDefaultToMailId());
                addEmails(ccEmailIds, settings.getShipmentAttachDefaultCCMailId());
            } else {
                addEmails(toEmailIds, settings.getConsolidationAttachDefaultToMailId());
                addEmails(ccEmailIds, settings.getConsolidationAttachDefaultCCMailId());
            }
        }
    }

    private void addEmails(Set<String> emailIds, String emailString) {
        if (!IsStringNullOrEmpty(emailString)) {
            List<String> emails = Arrays.stream(emailString.split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .collect(Collectors.toList());
            emailIds.addAll(emails);
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
        if (!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.REJECT_REMARKS, rejectRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, requestUser);
    }

    public void populateDictionaryForShipmentWithdraw(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String remarks,
                                                      Map<Integer, TenantModel> tenantModelMap) {
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(REGIONAL_BRANCH_CODE, tenantModelMap.get(shipmentDetails.getTenantId()).getCode());
        dictionary.put(REGIONAL_BRANCH_NAME, tenantModelMap.get(shipmentDetails.getTenantId()).getTenantName());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(Constants.CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.WITHDRAW_REMARKS, remarks);
        dictionary.put(USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForConsolidationWithdraw(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String remarks,
                                                           Map<Integer, TenantModel> tenantModelMap) {
        dictionary.put(CONSOLIDATION_CREATE_USER, consolidationDetails.getCreatedBy());
        dictionary.put(HUB_BRANCH_CODE, tenantModelMap.get(consolidationDetails.getTenantId()).getCode());
        dictionary.put(HUB_BRANCH_NAME, tenantModelMap.get(consolidationDetails.getTenantId()).getTenantName());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(Constants.WITHDRAW_REMARKS, remarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
    }

    public void populateDictionaryForShipmentDetach(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String detachRemarks) {
        dictionary.put(SHIPMENT_CREATE_USER, shipmentDetails.getCreatedBy());
        dictionary.put(SHIPMENT_ASSIGNED_USER, shipmentDetails.getAssignedTo());
        if (!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER, getConsolidationIdHyperLink(consolidationDetails.getConsolidationNumber(), consolidationDetails.getId()));
        dictionary.put(INTERBRANCH_CONSOLIDATION_NUMBER_WITHOUT_LINK, consolidationDetails.getConsolidationNumber());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(Constants.REJECT_REMARKS, detachRemarks);
        dictionary.put(ACTIONED_USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUESTED_USER_NAME, UserContext.getUser().getUsername());
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
        if (!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(shipmentDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (IsStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(shipmentDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, shipmentDetails.getCarrierDetails().getFlightNumber());
        if (!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(shipmentDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!IsStringNullOrEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(shipmentDetails.getCarrierDetails().getDestinationPort())) {
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
        if (!IsStringNullOrEmpty(shipmentDetails.getAssignedTo()))
            dictionary.put(SHIPMENT_ASSIGNED_USER_WITH_SLASH, "/ " + shipmentDetails.getAssignedTo());
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER, getShipmentIdHyperLink(shipmentDetails.getShipmentId(), shipmentDetails.getId()));
        dictionary.put(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK, shipmentDetails.getShipmentId());
        dictionary.put(Constants.CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(SOURCE_CONSOLIDATION_NUMBER, consolidationDetails.getConsolidationNumber());
        dictionary.put(MAWB_NUMBER, consolidationDetails.getMawb());
        dictionary.put(ETD_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEtd(), tsDateTimeFormat));
        dictionary.put(ETA_CAPS, convertToDPWDateFormat(consolidationDetails.getCarrierDetails().getEta(), tsDateTimeFormat));
        dictionary.put(LAT, consolidationDetails.getLatDate());
        if (!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getShippingLine()) && carrierMasterDataMap.containsKey(consolidationDetails.getCarrierDetails().getShippingLine())) {
            String carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getIataCode();
            if (IsStringNullOrEmpty(carrierCode))
                carrierCode = carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemValue();
            dictionary.put(CARRIER_CODE, carrierCode);
            dictionary.put(CARRIER_NAME, carrierMasterDataMap.get(consolidationDetails.getCarrierDetails().getShippingLine()).getItemDescription());
        }
        dictionary.put(FLIGHT_NUMBER1, consolidationDetails.getCarrierDetails().getFlightNumber());
        if (!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getOriginPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getOriginPort())) {
            dictionary.put(ReportConstants.POL, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getLocCode());
            dictionary.put(POL_NAME, unLocMap.get(consolidationDetails.getCarrierDetails().getOriginPort()).getName());
        }
        if (!IsStringNullOrEmpty(consolidationDetails.getCarrierDetails().getDestinationPort()) && unLocMap.containsKey(consolidationDetails.getCarrierDetails().getDestinationPort())) {
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
        if (unLocGuids == null || unLocGuids.isEmpty())
            return;
        Map<String, UnlocationsResponse> tempMap = masterDataUtils.getLocationData(new HashSet<>(unLocGuids));
        map.putAll(tempMap);
    }

    public void getCarriersData(List<String> carrierCodes, Map<String, CarrierMasterData> map) {
        if (carrierCodes == null || carrierCodes.isEmpty())
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
            if (!Objects.isNull(entry.getValue()) && !Objects.isNull(entry.getKey()))
                val = val.replace("{" + entry.getKey() + "}", entry.getValue().toString());
        }
        val = val.replaceAll("\\{.*?\\}", "");
        return val;
    }

    public void getEmailTemplate(Map<ShipmentRequestedType, EmailTemplatesRequest> response) {
        List<String> requests = new ArrayList<>(List.of(SHIPMENT_PULL_REQUESTED_EMAIL_TYPE, SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE, SHIPMENT_PUSH_REJECTED_EMAIL_TYPE, SHIPMENT_PULL_REJECTED_EMAIL_TYPE,
                SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE, SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE, SHIPMENT_DETACH_EMAIL_TYPE, SHIPMENT_PULL_WITHDRAW_EMAIL_TYPE, SHIPMENT_PUSH_WITHDRAW_EMAIL_TYPE));
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(Constants.TYPE));
        String operator = Operators.IN.getValue();
        List<Object> criteria1 = new ArrayList<>(List.of(field, operator, List.of(requests)));
        List<Object> criteria2 = new ArrayList<>(List.of(List.of(TENANTID), "=", TenantContext.getCurrentTenant()));
        request.setCriteriaRequests(List.of(criteria1, "and", criteria2));
        V1DataResponse v1DataResponse = iv1Service.getEmailTemplates(request);
        if (v1DataResponse != null) {
            List<EmailTemplatesRequest> emailTemplatesRequests = jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class);
            if (emailTemplatesRequests != null && !emailTemplatesRequests.isEmpty()) {
                populateEmailTemplateInResponseMap(response, emailTemplatesRequests);
            }
        }
    }

    private void populateEmailTemplateInResponseMap(Map<ShipmentRequestedType, EmailTemplatesRequest> response, List<EmailTemplatesRequest> emailTemplatesRequests) {
        for (EmailTemplatesRequest emailTemplatesRequest : emailTemplatesRequests) {
            ShipmentRequestedType shipmentRequestedType = EMAIL_TYPE_MAPPING.get(emailTemplatesRequest.getType());
            if (shipmentRequestedType != null) {
                response.put(shipmentRequestedType, emailTemplatesRequest);
            }
        }
    }

    public void getToAndCCEmailIdsFromTenantSettingsAndTenantsData(Set<Integer> tenantIds, Map<Integer, V1TenantSettingsResponse> tenantSettingsMap, Map<Integer, TenantModel> tenantsModelMap) {
        Map<Integer, Object> map = getTenantSettingsAndTenantsData(tenantIds);
        map.forEach((key, value) -> {
            TenantDetailsByListResponse.TenantDetails tenantDetails = modelMapper.map(value, TenantDetailsByListResponse.TenantDetails.class);
            tenantSettingsMap.put(key, modelMapper.map(tenantDetails.getTenantSettings(), V1TenantSettingsResponse.class));
            tenantsModelMap.put(key, modelMapper.map(tenantDetails.getTenant(), TenantModel.class));
        });
    }

    public Map<Integer, Object> getTenantSettingsAndTenantsData(Set<Integer> tenantIds) {
        if (tenantIds.isEmpty())
            return new HashMap<>();

        try {
            var v1Response = iv1Service.getTenantDetails(TenantDetailsByListRequest.builder().tenantIds(tenantIds.stream().toList()).take(100).build());
            return v1Response.getEntities()
                    .stream()
                    .collect(Collectors.groupingBy(
                            TenantDetailsByListResponse.TenantDetails::getTenantId,
                            Collectors.collectingAndThen(
                                    Collectors.toList(),
                                    list -> list.get(0)
                            )));
        } catch (Exception ex) {
            log.error(ex.getMessage());
            return new HashMap<>();
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
        } catch (Exception ex) {
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
        usernameEmailsMap.putAll(usersDtos.stream()
                .filter(user -> !IsStringNullOrEmpty(user.getEmail()))
                .collect(Collectors.toMap(UsersDto::getUsername, UsersDto::getEmail)));
    }

    // called when new dg pack is added or dg pack fields are changed or new dg container is added, or new pack added in dg container or dg container fields are changed
    public boolean changeShipmentDGStatusToReqd(ShipmentDetails shipmentDetails, boolean isDGClass1) {
        OceanDGStatus oldOceanDGStatus = shipmentDetails.getOceanDGStatus();
        if (Constants.IMP.equals(shipmentDetails.getDirection())) {
            shipmentDetails.setOceanDGStatus(null);
            return false;
        }

        if (Objects.isNull(shipmentDetails.getOceanDGStatus()) ||
                (!UserContext.isOceanDgUser() && (OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) ||
                        OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED.equals(shipmentDetails.getOceanDGStatus()) ||
                        OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED.equals(shipmentDetails.getOceanDGStatus()) ||
                        OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()))))
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_APPROVAL_REQUIRED);

        if ((OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) && !UserContext.isOceanDgCommercialUser()) ||
                (isDGClass1 && OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()))) {
            shipmentDetails.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED);
        }
        return !Objects.equals(oldOceanDGStatus, shipmentDetails.getOceanDGStatus());
    }

    public boolean checkIfDGClass1(String dgClass) {
        return !IsStringNullOrEmpty(dgClass) && dgClass.charAt(0) == '1';
    }

    public boolean checkIfAnyDGClass(String dgClass) throws RunnerException {
        if (!IsStringNullOrEmpty(dgClass)) {
            if (dgClass.charAt(0) == '7')
                throw new RunnerException("As per the DG SOP, you are not allowed to deal in Class 7 DG shipments");
            return true;
        }
        return false;
    }

    public boolean checkIfDGFieldsChangedInPacking(PackingRequest newPack, Packing oldPack) {
        if (!oldPack.getHazardous().equals(newPack.getHazardous()))
            return true;
        if (!Objects.equals(newPack.getDGClass(), oldPack.getDGClass()))
            return true;
        if (!Objects.equals(newPack.getUnNumber(), oldPack.getUnNumber()))
            return true;
        if (!Objects.equals(newPack.getProperShippingName(), oldPack.getProperShippingName()))
            return true;
        if (!Objects.equals(newPack.getPackingGroup(), oldPack.getPackingGroup()))
            return true;
        if (!compareBigDecimals(newPack.getMinimumFlashPoint(), oldPack.getMinimumFlashPoint()))
            return true;
        if (!Objects.equals(newPack.getMinimumFlashPointUnit(), oldPack.getMinimumFlashPointUnit()))
            return true;
        return !oldPack.getMarinePollutant().equals(newPack.getMarinePollutant());
    }

    public boolean compareBigDecimals(BigDecimal bd1, BigDecimal bd2) {
        // Check if both are null, they are considered equal
        if (Objects.equals(bd1, bd2)) {
            return true;
        }
        // If one is null and the other is not, they are not equal
        if (bd1 == null || bd2 == null) {
            return false;
        }
        // Use compareTo to ignore scale differences (0.00 vs 0.0)
        return bd1.compareTo(bd2) == 0;
    }

    public boolean checkIfDGFieldsChangedInContainer(ContainerRequest newContainer, Containers oldContainer) {
        if (!oldContainer.getHazardous().equals(newContainer.getHazardous()))
            return true;
        if (!Objects.equals(newContainer.getDgClass(), oldContainer.getDgClass()))
            return true;
        if (!Objects.equals(newContainer.getUnNumber(), oldContainer.getUnNumber()))
            return true;
        if (!Objects.equals(newContainer.getProperShippingName(), oldContainer.getProperShippingName()))
            return true;
        if (!Objects.equals(newContainer.getPackingGroup(), oldContainer.getPackingGroup()))
            return true;
        if (!compareBigDecimals(newContainer.getMinimumFlashPoint(), oldContainer.getMinimumFlashPoint()))
            return true;
        if (!Objects.equals(newContainer.getMinimumFlashPointUnit(), oldContainer.getMinimumFlashPointUnit()))
            return true;
        return !oldContainer.getMarinePollutant().equals(newContainer.getMarinePollutant());
    }

    public void updateUnLocData(CarrierDetails carrierDetails, CarrierDetails oldCarrierDetails) {
        try {
            if (!Objects.isNull(carrierDetails) && (Objects.isNull(oldCarrierDetails) || !Objects.equals(carrierDetails.getOrigin(), oldCarrierDetails.getOrigin())
                    || !Objects.equals(carrierDetails.getOriginPort(), oldCarrierDetails.getOriginPort())
                    || !Objects.equals(carrierDetails.getDestination(), oldCarrierDetails.getDestination())
                    || !Objects.equals(carrierDetails.getDestinationPort(), oldCarrierDetails.getDestinationPort()))) {
                Set<String> unlocoRequests = getUnlocoRequests(carrierDetails);
                Map<String, EntityTransferUnLocations> unlocationsMap = masterDataUtils.getLocationDataFromCache(unlocoRequests, EntityTransferConstants.LOCATION_SERVICE_GUID);
                EntityTransferUnLocations pol = unlocationsMap.get(carrierDetails.getOriginPort());
                EntityTransferUnLocations pod = unlocationsMap.get(carrierDetails.getDestinationPort());
                EntityTransferUnLocations origin = unlocationsMap.get(carrierDetails.getOrigin());
                EntityTransferUnLocations destination = unlocationsMap.get(carrierDetails.getDestination());
                if (!Objects.isNull(origin))
                    carrierDetails.setOriginLocCode(origin.getLocCode());
                if (!Objects.isNull(destination))
                    carrierDetails.setDestinationLocCode(destination.getLocCode());
                if (!Objects.isNull(pol))
                    carrierDetails.setOriginPortLocCode(pol.getLocCode());
                if (!Objects.isNull(pod))
                    carrierDetails.setDestinationPortLocCode(pod.getLocCode());
            }
        } catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierDetails.getId(), e.getMessage());
        }
    }

    private Set<String> getUnlocoRequests(CarrierDetails carrierDetails) {
        Set<String> unlocoRequests = new HashSet<>();
        if (!IsStringNullOrEmpty(carrierDetails.getOrigin()))
            unlocoRequests.add(carrierDetails.getOrigin());
        if (!IsStringNullOrEmpty(carrierDetails.getOriginPort()))
            unlocoRequests.add(carrierDetails.getOriginPort());
        if (!IsStringNullOrEmpty(carrierDetails.getDestination()))
            unlocoRequests.add(carrierDetails.getDestination());
        if (!IsStringNullOrEmpty(carrierDetails.getDestinationPort()))
            unlocoRequests.add(carrierDetails.getDestinationPort());
        return unlocoRequests;
    }

    public void updateCarrierUnLocData(CarrierDetails carrierDetails, Map<String, EntityTransferUnLocations> unlocationsMap) {
        try {
            EntityTransferUnLocations pol = unlocationsMap.get(carrierDetails.getOriginPort());
            EntityTransferUnLocations pod = unlocationsMap.get(carrierDetails.getDestinationPort());
            EntityTransferUnLocations origin = unlocationsMap.get(carrierDetails.getOrigin());
            EntityTransferUnLocations destination = unlocationsMap.get(carrierDetails.getDestination());
            if (!Objects.isNull(origin))
                carrierDetails.setOriginLocCode(origin.getLocCode());
            if (!Objects.isNull(destination))
                carrierDetails.setDestinationLocCode(destination.getLocCode());
            if (!Objects.isNull(pol))
                carrierDetails.setOriginPortLocCode(pol.getLocCode());
            if (!Objects.isNull(pod))
                carrierDetails.setDestinationPortLocCode(pod.getLocCode());
        } catch (Exception e) {
            log.error("Error while updating unlocCode for Carrier with Id {} due to {}", carrierDetails.getId(), e.getMessage());
        }
    }

    public void updateRoutingUnLocData(List<Routings> routingsList, Map<String, EntityTransferUnLocations> unlocationsMap) {
        try {
            for (var routing : routingsList) {
                EntityTransferUnLocations pol = unlocationsMap.get(routing.getPol());
                EntityTransferUnLocations pod = unlocationsMap.get(routing.getPod());
                if (!Objects.isNull(pol))
                    routing.setOriginPortLocCode(pol.getLocCode());
                if (!Objects.isNull(pod))
                    routing.setDestinationPortLocCode(pod.getLocCode());
            }
        } catch (Exception e) {
            log.error("Error while updating un-locCode for routing list {}", e.getMessage());
        }
    }

    public <T> void getChangedUnLocationFields(T newEntity, T oldEntity, Set<String> unlocationSet) {
        if (Objects.isNull(newEntity))
            return;
        Class<?> clazz = newEntity.getClass();
        try {
            for (Field field : clazz.getDeclaredFields()) {
                field.setAccessible(true);
                if (field.isAnnotationPresent(UnlocationData.class)) {
                    if ((Objects.isNull(oldEntity) || !Objects.equals(field.get(newEntity), field.get(oldEntity))) && !Objects.isNull(field.get(newEntity))) {
                        unlocationSet.add((String) field.get(newEntity));
                    }
                }

            }
        } catch (Exception e) {
            log.warn("Error while getting un-location fields for class {}", clazz.getSimpleName());
        }
    }

    public <T extends BaseEntity> void getChangedUnLocationFields(List<T> newEntityList, List<T> oldEntityList, Set<String> unlocationSet) {
        if (CollectionUtils.isEmpty(newEntityList))
            return;

        Map<Long, T> oldEntityMap = Optional.ofNullable(oldEntityList).orElse(Collections.emptyList()).stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));
        for (T newEntity : newEntityList) {
            getChangedUnLocationFields(newEntity, oldEntityMap.get(newEntity.getId()), unlocationSet);
        }
    }

    public String convertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat) {
        String strDate = "";
        if (date != null) {
            if (!IsStringNullOrEmpty(tsDatetimeFormat))
                strDate = date.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = date.format(getDPWDateFormatOrDefault());
        }
        return strDate;
    }

    public DateTimeFormatter getDPWDateFormatOrDefault() {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if (!CommonUtils.IsStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
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

    public void populateDictionaryForOceanDGApproval(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks,
                                                     TaskCreateResponse taskCreateResponse) {

        populateDictionaryForDGEmailFromShipment(dictionary, shipmentDetails, vesselsResponse, taskCreateResponse);
        populateDictionaryApprovalRequestForDGEmail(dictionary, remarks);
    }

    public void populateDictionaryForOceanDGCommercialApproval(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, String remarks, TaskCreateResponse taskCreateResponse) {
        populateDictionaryForOceanDGApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);
        List<AuditLog> auditLogList = iAuditLogDao.findByOperationAndParentId(
                DBOperationType.DG_APPROVE.name(), shipmentDetails.getId());
        if (auditLogList != null && auditLogList.size() != 0) {
            Map<String, AuditLogChanges> changesMap = auditLogList.get(0).getChanges();
            populateDGSenderDetailsFromAudit(changesMap, dictionary);
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

    public Integer getRoleId(OceanDGStatus oceanDGStatus) {
        String roleName = oceanDGStatus == OCEAN_DG_REQUESTED ? OCEAN_DG_ROLE : COMMERCIAL_OCEAN_DG_ROLE;
        return getRoleIDByRoleName(roleName, UserContext.getUser().getTenantId());
    }

    private Integer getRoleIDByRoleName(String roleName, Integer tenantId) {
        V1RoleIdRequest v1RoleIdRequest = V1RoleIdRequest
                .builder()
                .roleName(roleName)
                .tenantId(tenantId)
                .build();
        return iv1Service.getRoleIdsByRoleName(v1RoleIdRequest);
    }

    public List<String> getUserEmailsByRoleId(Integer roleId) {
        V1UsersEmailRequest request = new V1UsersEmailRequest();
        request.setRoleId(roleId);
        request.setTake(10);
        List<String> userEmailIds = new ArrayList<>();
        List<UsersRoleListResponse> userEmailResponse = iv1Service.getUserEmailsByRoleId(request);
        userEmailResponse.forEach(e -> userEmailIds.add(e.getEmail()));

        return userEmailIds;
    }

    public TaskCreateResponse createTask(ShipmentDetails shipmentDetails, Integer roleId)
            throws RunnerException {
        DGTaskCreateRequest taskRequest = DGTaskCreateRequest
                .builder()
                .entityType(SHIPMENTS_WITH_SQ_BRACKETS)
                .entityId(shipmentDetails.getId().toString())
                .roleId(roleId.toString())
                .taskType(OCEAN_DG_TASKTYPE)
                .taskStatus(PENDING_ACTION)
                .userId(UserContext.getUser().getUserId())
                .tenantId(UserContext.getUser().getTenantId().toString())
                .build();

        try {
            TaskCreateResponse taskCreateResponse = iv1Service.createTask(taskRequest);
            return taskCreateResponse;
        } catch (Exception e) {
            throw new RunnerException(String.format("Task creation failed for shipmentId: %s. Error: %s",
                    shipmentDetails.getId(), e.getMessage()));
        }
    }

    public void getVesselsData(CarrierDetails carrierDetails, VesselsResponse vesselsResponse) {
        if (carrierDetails == null) return;
        String guid = carrierDetails.getVessel();
        if (IsStringNullOrEmpty(guid)) {
            return;
        }
        List<Object> vesselCriteria = Arrays.asList(
                List.of(Constants.VESSEL_GUID_V1),
                "=",
                guid
        );
        CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(vesselCriteria).build();
        V1DataResponse v1DataResponse = iv1Service.fetchVesselData(vesselRequest);
        List<VesselsResponse> vesselsResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, VesselsResponse.class);

        if (vesselsResponseList != null && !vesselsResponseList.isEmpty()) {
            vesselsResponse.setName(vesselsResponseList.get(0).getName());
        }

    }

    private void populateDictionaryForDGEmailFromShipment(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, VesselsResponse vesselsResponse, TaskCreateResponse taskCreateResponse) {
        if (shipmentDetails.getCarrierDetails() != null) {
            dictionary.put(ORIGIN_PORT, shipmentDetails.getCarrierDetails().getOriginPort());
            dictionary.put(DESTINATION_PORT, shipmentDetails.getCarrierDetails().getDestinationPort());
            dictionary.put(CARRIER, shipmentDetails.getCarrierDetails().getShippingLine());
            dictionary.put(VOYAGE, shipmentDetails.getCarrierDetails().getVoyage());
            if (shipmentDetails.getCarrierDetails().getEta() != null) {
                dictionary.put(ETA, shipmentDetails.getCarrierDetails().getEta().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
            }

            if (shipmentDetails.getCarrierDetails().getEtd() != null) {
                dictionary.put(ETD, shipmentDetails.getCarrierDetails().getEtd()
                        .format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
            }
        }
        dictionary.put(TRANSPORT_MODE, shipmentDetails.getTransportMode());
        dictionary.put(SHIPMENT_TYPE, shipmentDetails.getDirection());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(CARGO_TYPE, shipmentDetails.getShipmentType());
        if (vesselsResponse != null) {
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


        String dgPackageTypeAndCount = Optional.ofNullable(shipmentDetails.getPackingList())
                .orElse(List.of())  // If the list is null, use an empty list
                .stream()
                .filter(Packing::getHazardous)
                .map(packing -> packing.getPacks() + " " + packing.getPacksType())
                .collect(Collectors.joining(", "));

        String packagesTypeAndCount = Optional.ofNullable(shipmentDetails.getPackingList())
                .orElse(List.of())  // If the list is null, use an empty list
                .stream()
                .map(packing -> packing.getPacks() + " " + packing.getPacksType())
                .collect(Collectors.joining(", "));

        dictionary.put(DG_PACKAGES_TYPE, dgPackageTypeAndCount);
        dictionary.put(TOTAL_PACKAGES_TYPE, packagesTypeAndCount);
        dictionary.put(VIEWS, getTaskIdHyperLink(shipmentDetails.getShipmentId(), taskCreateResponse.getTasksId()));
    }

    private void populateDictionaryApprovalRequestForDGEmail(Map<String, Object> dictionary, String remarks) {
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(USER_NAME, UserContext.getUser().getUsername());
        dictionary.put(REQUEST_DATE_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
        dictionary.put(REQUESTER_REMARKS, remarks);
    }

    private void populateDGReceiverDictionary(Map<String, Object> dictionary, ShipmentDetails shipmentDetails, OceanDGRequest request) {
        dictionary.put(USER_BRANCH, UserContext.getUser().getTenantDisplayName());
        dictionary.put(USER_COUNTRY, UserContext.getUser().getTenantCountryCode());
        dictionary.put(SHIPMENT_NUMBER, shipmentDetails.getShipmentId());
        dictionary.put(APPROVER_NAME, UserContext.getUser().getUsername());
        dictionary.put(APPROVED_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
        dictionary.put(REMARKS, request.getRemarks());
        dictionary.put(STATUS, request.getStatus());
    }

    private void populateDGSenderDetailsFromAudit(Map<String, AuditLogChanges> changesMap, Map<String, Object> dictionary) {

        for (AuditLogChanges change : changesMap.values()) {
            if (change.getFieldName().equalsIgnoreCase(TIME)) {
                dictionary.put(DG_APPROVER_TIME, change.getNewValue());
            } else if (change.getFieldName().equalsIgnoreCase(USERNAME)) {
                dictionary.put(DG_APPROVER_NAME, change.getNewValue());
            }
        }
    }

    public void removeDuplicateTrackingEvents(List<Events> events) {
        if (events == null || events.isEmpty()) {
            return;
        }

        Set<String> uniqueKeys = new HashSet<>();

        events.removeIf(event -> !uniqueKeys.add(
                getTrackingEventsUniqueKey(
                        event.getEventCode(),
                        event.getContainerNumber(),
                        event.getShipmentNumber(),
                        event.getSource(),
                        event.getPlaceName()
                )
        ));
    }

    public String getTrackingEventsUniqueKey(String eventCode, String containerNumber, String shipmentNumber, String source, String placeName) {
        return String.join("-",
                StringUtils.defaultString(eventCode),
                StringUtils.defaultString(containerNumber),
                StringUtils.defaultString(shipmentNumber),
                StringUtils.defaultString(source),
                StringUtils.defaultString(placeName)
        );
    }

    public static String getKey(String... args) {
        return String.join("-", args);
    }

    // This method is used to validate whether the transport mode is allowed to be used or not
    public boolean isTransportModeValid(String transportMode, String entity, V1TenantSettingsResponse tenantSettings) {
        Set<String> validTransportModes;

        switch (entity) {
            case CUSTOMER_BOOKING ->
                    validTransportModes = this.getValidTransportModes(tenantSettings.getBookingTransportModeAir(), tenantSettings.getBookingTransportModeSea(), tenantSettings.getBookingTransportModeRail(), tenantSettings.getBookingTransportModeRoad());
            case SHIPMENT_DETAILS ->
                    validTransportModes = this.getValidTransportModes(tenantSettings.getShipmentTransportModeAir(), tenantSettings.getShipmentTransportModeSea(), tenantSettings.getShipmentTransportModeRail(), tenantSettings.getShipmentTransportModeRoad());
            default -> validTransportModes = new HashSet<>();
        }

        return validTransportModes.contains(transportMode);
    }

    /**
     * This method will return list of valid transport modes based on the tenant configs
     *
     * @param isAirValid  if true, add AIR in valid modes list
     * @param isSeaValid  if true, add SEA in valid modes list
     * @param isRailValid if true, add RAI in valid modes list
     * @param isRoadValid if true, add ROA in valid modes list
     * @return Set of valid transport modes
     */
    private Set<String> getValidTransportModes(Boolean isAirValid, Boolean isSeaValid, Boolean isRailValid, Boolean isRoadValid) {

        Set<String> transportModes = new HashSet<>();

        if (Boolean.TRUE.equals(isAirValid))
            transportModes.add(Constants.TRANSPORT_MODE_AIR);

        if (Boolean.TRUE.equals(isSeaValid))
            transportModes.add(Constants.TRANSPORT_MODE_SEA);

        if (Boolean.TRUE.equals(isRailValid))
            transportModes.add(Constants.TRANSPORT_MODE_RAI);

        if (Boolean.TRUE.equals(isRoadValid))
            transportModes.add(Constants.TRANSPORT_MODE_ROA);

        return transportModes;
    }

    public void createMasterDataKeysList(Set<MasterListRequest> masterListRequests, Set<String> keys) {
        if (Objects.isNull(masterListRequests))
            return;
        for (MasterListRequest masterListRequest : masterListRequests) {
            keys.add(masterListRequest.ItemValue + '#' + MasterDataType.getNameFromDescription(masterListRequest.ItemType));
        }
    }

    /**
     * Updates the given list of events with description and direction from master data.
     *
     * <p>Filters events without IDs and with non-null event codes, fetches corresponding
     * master data, and updates the description and direction if matching data is found.
     * Preserves original values if no match is available.</p>
     *
     * @param eventsList the list of {@link Events} to update; does nothing if null or empty.
     */
    public void updateEventWithMasterData(List<Events> eventsList) {
        if (CollectionUtils.isEmpty(eventsList))
            return;
        // Create a map of event codes to their corresponding master data entries.
        // Filter out events with non-null IDs and null event codes before mapping.
        Map<String, EntityTransferMasterLists> eventCodeMasterDataMap = getEventCodeMasterDataMap(eventsList.stream()
                .filter(i -> Objects.isNull(i.getId()))
                .map(Events::getEventCode)
                .filter(Objects::nonNull).toList());

        eventsList.forEach(event -> {
            EntityTransferMasterLists masterData = eventCodeMasterDataMap.get(event.getEventCode());
            // If master data is found, update the event's description and direction.
            if (masterData != null) {
                event.setDescription(masterData.getItemDescription());
                if (masterData.getIdentifier3() != null) {
                    event.setDirection(masterData.getIdentifier3());
                }
            }
            // If no master data is found, retain the original description and direction.
        });
    }

    private Map<String, EntityTransferMasterLists> getEventCodeMasterDataMap(List<String> eventCodes) {
        Map<String, EntityTransferMasterLists> eventCodeMasterDataMap = new HashMap<>();
        log.info("EventService: received {} event codes for fetching Masterdata", eventCodes.size());
        if (CollectionUtils.isEmpty(eventCodes))
            return eventCodeMasterDataMap;
        try {
            List<Object> masterDataListCriteria = Arrays.asList(
                    List.of(
                            List.of(MasterDataConstants.ITEM_TYPE),
                            "=",
                            MasterDataType.ORDER_EVENTS.getId()
                    ),
                    "and",
                    List.of(
                            List.of(MasterDataConstants.ITEM_VALUE),
                            "IN",
                            List.of(eventCodes)
                    )
            );
            CommonV1ListRequest v1ListRequest = CommonV1ListRequest.builder().criteriaRequests(masterDataListCriteria).build();
            var v1DataResponse = iv1Service.fetchMasterData(v1ListRequest);
            List<EntityTransferMasterLists> masterData = jsonHelper.convertValueToList(v1DataResponse.getEntities(), EntityTransferMasterLists.class);
            masterData.forEach(i -> eventCodeMasterDataMap.put(i.getItemValue(), i));
        } catch (Exception e) {
            log.error("EventService : Error fetching masterdata for event codes", e);
        }

        return eventCodeMasterDataMap;
    }

    public boolean checkIfPartyExists(PartiesResponse party) {
        return !Objects.isNull(party) && !IsStringNullOrEmpty(party.getOrgCode());
    }

    public boolean checkIfPartyExists(Parties party) {
        return !Objects.isNull(party) && !IsStringNullOrEmpty(party.getOrgCode());
    }

    public String getCountryFromUnLocCode(String unLocCode) {
        if (IsStringNullOrEmpty(unLocCode) || unLocCode.length() < 2)
            return null;
        return getAlpha3FromAlpha2(unLocCode.substring(0, 2));
    }

    public void checkForMandatoryHsCodeForUAE(Awb awb) {
        String destinationPortLocCode = null;
        if (awb.getShipmentId() != null) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(awb.getShipmentId());
            if (shipmentDetails.isPresent())
                destinationPortLocCode = shipmentDetails.get().getCarrierDetails().getDestinationPortLocCode();
        } else if (awb.getConsolidationId() != null) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(awb.getConsolidationId());
            if (consolidationDetails.isPresent())
                destinationPortLocCode = consolidationDetails.get().getCarrierDetails().getDestinationPortLocCode();
        }

        if (destinationPortLocCode != null && destinationPortLocCode.startsWith(UAE_TWO_DIGIT_IATA_CODE)) {
            List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfoList = awb.getAwbGoodsDescriptionInfo();
            awbGoodsDescriptionInfoList.forEach(awbGoodsDescriptionInfo -> {
                if (Objects.isNull(awbGoodsDescriptionInfo.getHsCode())) {
                    throw new ValidationException("Please enter the HS code in the goods description of the cargo information tab.");
                }
            });
        }
    }

    public String getAutoPopulateDepartment(String transportMode, String direction, String module) {
        String department = null;
        List<Map<String, Object>> departmentList = mdmServiceAdapter.getDepartmentList(transportMode, direction, module);
        if (!CollectionUtils.isEmpty(departmentList)) {
            List<String> uniqueDepartments = departmentList.stream()
                    .map(i -> StringUtility.convertToString(i.get(MdmConstants.DEPARTMENT)))
                    .distinct().toList();
            department = uniqueDepartments.size() == 1 ? StringUtility.convertToString(uniqueDepartments.get(0)) : null;
        }
        return department;
    }

    public ShipmentDetailsResponse getShipmentDetailsResponse(ShipmentDetails shipmentDetails, List<String> includeColumns) {
        return setIncludedFields(shipmentDetails, includeColumns);
    }

    private ShipmentDetailsResponse setIncludedFields(ShipmentDetails shipmentDetail, List<String> includeColumns) {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();

        includeColumns.forEach(field -> {
            try {
                String capitalizedField = capitalize(field);

                // Reflectively obtain the getter and setter methods once
                Method getter = ShipmentDetails.class.getMethod("get" + capitalizedField);
                Object value = getter.invoke(shipmentDetail);

                Object dtoValue = getDtoValue(value);
                Class<?> paramType;
                if (dtoValue instanceof List<?> list && !list.isEmpty()) {
                    paramType = List.class;
                } else if (dtoValue != null) {
                    paramType = dtoValue.getClass();
                } else {
                    paramType = getter != null ? getter.getReturnType() : Object.class;
                }

                Method setter = ShipmentDetailsResponse.class.getMethod("set" + capitalizedField, paramType);
                setter.invoke(shipmentDetailsResponse, dtoValue != null ? dtoValue : value);
            } catch (Exception e) {
                log.error("No such field: {}", field, e);
            }
        });
        return shipmentDetailsResponse;
    }

    @Nullable
    public Object getDtoValue(Object value) {
        Object dtoValue = null;
        if (value instanceof CarrierDetails) {
            dtoValue = modelMapper.map(value, CarrierDetailResponse.class);
        } else if (value instanceof AdditionalDetails) {
            dtoValue = modelMapper.map(value, AdditionalDetailResponse.class);
        } else if (value instanceof PickupDeliveryDetails) {
            dtoValue = modelMapper.map(value, PickupDeliveryDetailsResponse.class);
        } else if (value instanceof Parties) {
            dtoValue = modelMapper.map(value, PartiesResponse.class);
        }

        if (value instanceof List<?>) {
            List<?> list = (List<?>) value;
            if (!list.isEmpty()) {
                Class<?> firstElementClass = list.get(0).getClass();
                Type targetType = getTypeTokenMap().get(firstElementClass);
                if (targetType != null) {
                    dtoValue = modelMapper.map(value, targetType);
                }
            }
        }
        return dtoValue;
    }

    private Map<Class<?>, Type> getTypeTokenMap() {
        Map<Class<?>, Type> typeTokenMap = new HashMap<>();
        typeTokenMap.put(Containers.class, new TypeToken<List<ContainerResponse>>() {
        }.getType());
        typeTokenMap.put(BookingCarriage.class, new TypeToken<List<BookingCarriageResponse>>() {
        }.getType());
        typeTokenMap.put(ELDetails.class, new TypeToken<List<ELDetailsResponse>>() {
        }.getType());
        typeTokenMap.put(Events.class, new TypeToken<List<EventsResponse>>() {
        }.getType());
        typeTokenMap.put(Packing.class, new TypeToken<List<PackingResponse>>() {
        }.getType());
        typeTokenMap.put(ReferenceNumbers.class, new TypeToken<List<ReferenceNumbersResponse>>() {
        }.getType());
        typeTokenMap.put(Routings.class, new TypeToken<List<RoutingsResponse>>() {
        }.getType());
        typeTokenMap.put(ServiceDetails.class, new TypeToken<List<ServiceDetailsResponse>>() {
        }.getType());
        typeTokenMap.put(TruckDriverDetails.class, new TypeToken<List<TruckDriverDetailsResponse>>() {
        }.getType());
        typeTokenMap.put(Notes.class, new TypeToken<List<NotesResponse>>() {
        }.getType());
        typeTokenMap.put(Jobs.class, new TypeToken<List<JobResponse>>() {
        }.getType());
        typeTokenMap.put(ConsolidationDetails.class, new TypeToken<List<ConsolidationListResponse>>() {
        }.getType());
        typeTokenMap.put(Parties.class, new TypeToken<List<PartiesResponse>>() {
        }.getType());
        typeTokenMap.put(ShipmentOrder.class, new TypeToken<List<ShipmentOrderResponse>>() {
        }.getType());
        typeTokenMap.put(CarrierDetails.class, new TypeToken<List<CarrierDetailResponse>>() {
        }.getType());
        return typeTokenMap;
    }


    private String capitalize(String str) {
        if (str == null || str.isEmpty()) return str;
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

    public List<Long> getTriangulationPartnerList(List<TriangulationPartner> partnerList) {
        return partnerList != null ? partnerList.stream()
                .filter(Objects::nonNull)
                .map(TriangulationPartner::getTriangulationPartner)
                .toList() : Collections.emptyList();
    }

    public List<Long> getTenantIdsFromEntity(ListCousinBranchesForEtRequest request) {

        Set<Long> tenantIds = new HashSet<>();
        Long sourceTenantId = TenantContext.getCurrentTenant().longValue();
        Long receivingBranch = null;
        List<Long> triangulationPartners = null;
        List<Long> otherIds = new ArrayList<>();
        Optional<ShipmentDetails> optionalShipmentDetails;
        Optional<ConsolidationDetails> optionalConsolidationDetails;

        if (Objects.equals(request.getEntityType(), Constants.SHIPMENT) && (request.getEntityId() != null || request.getEntityGuid() != null)) {
            optionalShipmentDetails = getShipmentDetails(request);

            if (optionalShipmentDetails.isEmpty()) {
                return new ArrayList<>();
            }
            ShipmentDetails shipmentDetails = optionalShipmentDetails.get();
            sourceTenantId = Long.valueOf(shipmentDetails.getTenantId());

            if (Boolean.TRUE.equals(request.getIsReassign())) {
                receivingBranch = shipmentDetails.getReceivingBranch();
                triangulationPartners = extractTriangulationPartners(shipmentDetails.getTriangulationPartnerList());
            }

            processConsolidationDetails(shipmentDetails, request.getIsReceivingBranch(), request.getIsTriangulationBranch(), otherIds);
        } else if (Objects.equals(request.getEntityType(), Constants.CONSOLIDATION) && (request.getEntityId() != null || request.getEntityGuid() != null)) {
            optionalConsolidationDetails = getConsolidationDetails(request);
            if (optionalConsolidationDetails.isEmpty()) {
                return new ArrayList<>();
            }
            ConsolidationDetails consolidationDetails = optionalConsolidationDetails.get();
            sourceTenantId = Long.valueOf(consolidationDetails.getTenantId());

            if (Boolean.TRUE.equals(request.getIsReassign())) {
                receivingBranch = consolidationDetails.getReceivingBranch();
                triangulationPartners = extractTriangulationPartners(consolidationDetails.getTriangulationPartnerList());
            }

            processConsolidationShipments(consolidationDetails, request.getIsReceivingBranch(), request.getIsTriangulationBranch(), otherIds);
        }

        tenantIds.add(sourceTenantId);
        if (receivingBranch != null) {
            tenantIds.add(receivingBranch);
        }
        addAllIfNotEmpty(tenantIds, triangulationPartners);
        addAllIfNotEmpty(tenantIds, otherIds);
        return tenantIds.stream().filter(Objects::nonNull).toList();
    }

    private Optional<ShipmentDetails> getShipmentDetails(ListCousinBranchesForEtRequest request) {
        Optional<ShipmentDetails> optionalShipmentDetails;
        if (request.getEntityId() != null) {
            optionalShipmentDetails = shipmentDao.findShipmentByIdWithQuery(request.getEntityId());
        } else {
            optionalShipmentDetails = shipmentDao.findShipmentByGuidWithQuery(UUID.fromString(request.getEntityGuid()));
        }
        return optionalShipmentDetails;
    }

    private Optional<ConsolidationDetails> getConsolidationDetails(ListCousinBranchesForEtRequest request) {
        Optional<ConsolidationDetails> optionalConsolidationDetails;
        if (request.getEntityId() != null) {
            optionalConsolidationDetails = consolidationDetailsDao.findConsolidationByIdWithQuery(request.getEntityId());
        } else {
            optionalConsolidationDetails = consolidationDetailsDao.findConsolidationByGuidWithQuery(UUID.fromString(request.getEntityGuid()));
        }
        return optionalConsolidationDetails;
    }

    private void addAllIfNotEmpty(Set<Long> set, List<Long> list) {
        if (list != null && !list.isEmpty()) {
            set.addAll(list);
        }
    }

    private void processConsolidationDetails(ShipmentDetails shipmentDetails, Boolean isReceivingBranch, Boolean isTriangulationBranch, List<Long> otherIds) {
        Set<ConsolidationDetails> consolidationList = shipmentDetails.getConsolidationList();
        if (consolidationList != null && !consolidationList.isEmpty()) {
            ConsolidationDetails consolidationDetails = consolidationList.iterator().next();
            if (Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
                if (Boolean.TRUE.equals(isReceivingBranch)) {
                    otherIds.add(Long.valueOf(consolidationDetails.getTenantId()));
                    otherIds.addAll(extractTriangulationPartners(consolidationDetails.getTriangulationPartnerList()));
                    consolidationDetails.getShipmentsList().forEach(s -> otherIds.add(Long.valueOf(s.getTenantId())));
                }
                if (Boolean.TRUE.equals(isTriangulationBranch)) {
                    otherIds.add(consolidationDetails.getReceivingBranch());
                    otherIds.addAll(extractTriangulationPartners(consolidationDetails.getTriangulationPartnerList()));
                    consolidationDetails.getShipmentsList().forEach(s -> {
                        otherIds.add(Long.valueOf(s.getTenantId()));
                        if (!Objects.equals(s.getId(), shipmentDetails.getId())) {
                            otherIds.add(s.getReceivingBranch());
                        }
                    });
                }
            }
        }
    }

    private void processConsolidationShipments(ConsolidationDetails consolidationDetails, Boolean isReceivingBranch, Boolean isTriangulationBranch, List<Long> otherIds) {
        if (Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole())) {
            if (Boolean.TRUE.equals(isReceivingBranch)) {
                consolidationDetails.getShipmentsList().forEach(s -> otherIds.add(Long.valueOf(s.getTenantId())));
            }
            if (Boolean.TRUE.equals(isTriangulationBranch)) {
                consolidationDetails.getShipmentsList().forEach(s -> {
                    otherIds.add(s.getReceivingBranch());
                    otherIds.add(Long.valueOf(s.getTenantId()));
                });
            }
        }
    }

    private List<Long> extractTriangulationPartners(List<TriangulationPartner> partners) {
        return Optional.ofNullable(partners)
                .orElse(Collections.emptyList())
                .stream()
                .map(TriangulationPartner::getTriangulationPartner)
                .toList();
    }

    public Parties removeIdFromParty(Parties parties) {
        if (parties == null || IsStringNullOrEmpty(parties.getOrgId()))
            return null;
        PartiesRequest partiesRequest = jsonHelper.convertValue(parties, PartiesRequest.class);
        partiesRequest.setId(null);
        return jsonHelper.convertValue(partiesRequest, Parties.class);
    }

    public static boolean checkSameParties(Parties obj1, Parties obj2) {
        if (obj1 == null && obj2 == null) return true;
        if (obj1 == null || obj2 == null) return false;

        return Objects.equals(obj1.getOrgId(), obj2.getOrgId()) &&
                Objects.equals(obj1.getAddressId(), obj2.getAddressId());
    }

    public static boolean checkPartyNotNull(Parties party) {
        if (party == null) return false;
        else if (party.getOrgId() == null) return false;
        else return !IsStringNullOrEmpty(party.getOrgId());
    }

    public static boolean checkAddressNotNull(Parties party) {
        if (party == null) return false;
        else if (IsStringNullOrEmpty(party.getOrgId())) return false;
        else return !IsStringNullOrEmpty(party.getAddressId());
    }

    public static boolean checkAddressNotNull(PartiesResponse party) {
        if (party == null) return false;
        else if (IsStringNullOrEmpty(party.getOrgId())) return false;
        else return !IsStringNullOrEmpty(party.getAddressId());
    }

    public Long getReceivingBranch(String orgIdString, String addressIdString) {
        Long orgId = Long.valueOf(orgIdString);
        Long addressId = Long.valueOf(addressIdString);
        TenantFilterRequest request = TenantFilterRequest.builder().orgId(orgId).addressId(addressId).build();
        V1DataResponse response = iv1Service.listBranchesByDefaultOrgAndAddress(request);
        if (Objects.nonNull(response.getEntities())) {
            List<V1TenantResponse> tenantResponses = jsonHelper.convertValueToList(response.getEntities(), V1TenantResponse.class);
            if (!tenantResponses.isEmpty())
                return tenantResponses.get(0).getTenantId();
        }
        return null;
    }

    public static boolean checkAirSecurityForShipment(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getDirection().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public static boolean checkAirSecurityForConsolidation(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidationDetails.getShipmentType().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public static boolean checkAirSecurityForBooking(CustomerBooking customerBooking) {
        if (customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_AIR) && customerBooking.getDirection().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public static boolean checkAirSecurityForBookingRequest(CustomerBookingRequest customerBooking) {
        if (customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_AIR) && customerBooking.getDirection().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    public EventsRequest prepareEventRequest(Long entityId, String eventCode, String entityType, String referenceNumber) {
        EventsRequest eventsRequest = new EventsRequest();
        eventsRequest.setActual(getUserZoneTime(LocalDateTime.now()));
        eventsRequest.setEntityId(entityId);
        eventsRequest.setEntityType(entityType);
        eventsRequest.setEventCode(eventCode);
        if (!CommonUtils.IsStringNullOrEmpty(referenceNumber))
            eventsRequest.setContainerNumber(referenceNumber);
        eventsRequest.setIsPublicTrackingEvent(true);
        eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        return eventsRequest;
    }

    public LocalDateTime getUserZoneTime(LocalDateTime inputDateTime) {
        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = Optional.ofNullable(userDetails.getEnableTimeZone()).orElse(false);
        String tenantTimeZone = userDetails.getTimeZoneId();
        return DateUtils.convertDateToUserTimeZone(inputDateTime, null, tenantTimeZone, enableTimeZoneFlag);
    }

    public void impersonateUser(Integer tenantId) {
        try {
            Map<Integer, Object> responseMap = getTenantSettingsAndTenantsData(Set.of(tenantId));
            TenantDetailsByListResponse.TenantDetails response = modelMapper.map(responseMap.get(tenantId), TenantDetailsByListResponse.TenantDetails.class);
            TenantModel tenantModel = modelMapper.map(response.getTenant(), TenantModel.class);
            UserContext.getUser().setEnableTimeZone(tenantModel.getEnableTimeZone());
            UserContext.getUser().setTimeZoneId(tenantModel.getTimeZoneId());
            log.info("User impersonated successfully");
        } catch (Exception e) {
            // in case of any failure set Enable Time zone and time zone id as null to avoid wrong transformations
            // UTC will be the fallback value
            UserContext.getUser().setEnableTimeZone(null);
            UserContext.getUser().setTimeZoneId(null);
            log.warn("Error while impersonating user with tenant Id {}", tenantId, e);
        }
    }

    public Object setIncludedFieldsToResponse(Object entity, Set<String> includeColumns, Object response) {
        includeColumns.forEach(field -> {
            try {
                Object value = getNestedFieldValue(entity, field); // Get nested field value
                if (value == null) {
                    return;
                }

                Object dtoValue = mapToDTO(value); // Convert to DTO if necessary
                setNestedFieldValue(response, field, dtoValue != null ? dtoValue : value);
            } catch (Exception e) {
                log.error("No such field: {}", field, e);
            }
        });

        return response;
    }

    public Object mapToDTO(Object value) {
        if (value instanceof CarrierDetails) {
            return modelMapper.map(value, CarrierDetailResponse.class);
        } else if (value instanceof AdditionalDetails) {
            return modelMapper.map(value, AdditionalDetailResponse.class);
        } else if (value instanceof PickupDeliveryDetails) {
            return modelMapper.map(value, PickupDeliveryDetailsResponse.class);
        } else if (value instanceof Parties) {
            return modelMapper.map(value, PartiesResponse.class);
        } else if (value instanceof ArrivalDepartureDetails) {
            return modelMapper.map(value, ArrivalDepartureDetailsResponse.class);
        } else if (value instanceof List<?>) {
            return mapListToDTO(value);
        } else if (value instanceof Set) {
            return mapListToDTOSet(value);
        }
        return value; // Return as is if not mappable
    }

    public Object mapListToDTOSet(Object value) {
        Set<?> set = (Set<?>) value;
        if (set.isEmpty()) return value;
        Object obj = set.iterator().next();
        if (obj instanceof Containers) {
            return modelMapper.map(value, new TypeToken<Set<ContainerResponse>>() {
            }.getType());
        } else if (obj instanceof ConsolidationDetails) {
            return modelMapper.map(value, new TypeToken<Set<ConsolidationListResponse>>() {
            }.getType());
        }
        return value;
    }

    public Object mapListToDTO(Object value) {
        List<?> list = (List<?>) value;
        if (list.isEmpty()) return value;

        if (list.get(0) instanceof Containers) {
            return modelMapper.map(value, new TypeToken<List<ContainerResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof BookingCarriage) {
            return modelMapper.map(value, new TypeToken<List<BookingCarriageResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof ELDetails) {
            return modelMapper.map(value, new TypeToken<List<ELDetailsResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof Events) {
            return modelMapper.map(value, new TypeToken<List<EventsResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof Packing) {
            return modelMapper.map(value, new TypeToken<List<PackingResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof ReferenceNumbers) {
            return modelMapper.map(value, new TypeToken<List<ReferenceNumbersResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof Routings) {
            return modelMapper.map(value, new TypeToken<List<RoutingsResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof ServiceDetails) {
            return modelMapper.map(value, new TypeToken<List<ServiceDetailsResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof TruckDriverDetails) {
            return modelMapper.map(value, new TypeToken<List<TruckDriverDetailsResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof Notes) {
            return modelMapper.map(value, new TypeToken<List<NotesResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof Jobs) {
            return modelMapper.map(value, new TypeToken<List<JobResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof ConsolidationDetails) {
            return modelMapper.map(value, new TypeToken<List<ConsolidationListResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof Parties) {
            return modelMapper.map(value, new TypeToken<List<PartiesResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof ShipmentOrder) {
            return modelMapper.map(value, new TypeToken<List<ShipmentOrderResponse>>() {
            }.getType());
        } else if (list.get(0) instanceof TriangulationPartner) {
            return modelMapper.map(value, new TypeToken<List<TriangulationPartnerResponse>>() {
            }.getType());
        }
        return value; // Return as is if no mapping exists
    }

    public void setNestedFieldValue(Object object, String fieldPath, Object value) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, InstantiationException {
        String[] fields = fieldPath.split("\\.");
        Object target = object;

        for (int i = 0; i < fields.length - 1; i++) {
            Method getter;
            try {
                getter = target.getClass().getMethod("get" + capitalizeV3(fields[i]));
            } catch (NoSuchMethodException e) {
                // If no getter exists, assume it's a Map field
                if (target instanceof Map) {
                    Map<String, Object> mapTarget = (Map<String, Object>) target;
                    mapTarget.putIfAbsent(fields[i], new HashMap<>());
                    target = mapTarget.get(fields[i]);
                    continue;
                } else {
                    throw e; // Rethrow exception if it's not a map
                }
            }
            Object nextTarget = getter.invoke(target);

            if (nextTarget == null) {
                Method setter = target.getClass().getMethod("set" + capitalizeV3(fields[i]), getter.getReturnType());
                if (Map.class.isAssignableFrom(getter.getReturnType())) {
                    nextTarget = new HashMap<>(); // Initialize Map
                } else {
                    nextTarget = getter.getReturnType().getDeclaredConstructor().newInstance();
                }
                setter.invoke(target, nextTarget);
            }
            target = nextTarget;
        }

        String lastField = fields[fields.length - 1];

        setTargetValue(value, target, lastField);
    }

    public void setTargetValue(Object value, Object target, String lastField) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        if (target instanceof Map) {
            ((Map<String, Object>) target).put(lastField, value);
        } else {
            Method setter;
            try {
                // Use Map.class for flexibility in map types
                if (value instanceof Map) {
                    setter = target.getClass().getMethod("set" + capitalize(lastField), Map.class);
                } else if (value instanceof List) {
                    setter = target.getClass().getMethod("set" + capitalize(lastField), List.class);
                } else if (value instanceof Set) {
                    setter = target.getClass().getMethod("set" + capitalize(lastField), Set.class);
                } else {
                    setter = target.getClass().getMethod("set" + capitalize(lastField), value.getClass());
                }
                setter.invoke(target, value);
            } catch (NoSuchMethodException e) {
                throw new NoSuchMethodException("No setter found for field: " + lastField + " in " + target.getClass().getSimpleName());
            }
        }
    }

    /**
     * Recursively gets a nested field value using reflection.
     */
    public Object getNestedFieldValue(Object object, String fieldPath) throws NoSuchMethodException {
        String[] fields = fieldPath.split("\\.");
        Object value = object;

        for (String field : fields) {
            if (value == null) {
                return null;
            }
            try {
                // Attempt to get value using getter method
                Method getter = value.getClass().getMethod("get" + capitalizeV3(field));
                value = getter.invoke(value);
            } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
                // If no getter exists, check if it's a Map and retrieve value by key
                if (value instanceof Map) {
                    value = ((Map<?, ?>) value).get(field);
                } else {
                    throw new NoSuchMethodException("No getter found for field: " + field + " in " + value.getClass().getSimpleName());
                }
            }
        }
        return value;
    }

    /**
     * Capitalizes the first letter of a string.
     */
    private String capitalizeV3(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

    public static List<String> splitAndTrimStrings(String input) {
        if (input == null || input.isEmpty()) {
            return List.of();
        }

        return Arrays.stream(input.split(","))
                .map(String::trim)
                .toList();
    }

    public static void includeRequiredColumns(Set<String> includeColumns) {
        includeColumns.add("id");
        includeColumns.add("guid");
    }

}