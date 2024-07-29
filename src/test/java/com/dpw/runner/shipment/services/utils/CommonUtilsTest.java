package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.commons.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.*;
import com.itextpdf.text.exceptions.InvalidPdfException;
import com.itextpdf.text.pdf.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.transaction.TransactionSystemException;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.*;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class CommonUtilsTest {

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ExecutorService syncExecutorService;

    @Mock
    private ObjectMapper mapper;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @InjectMocks
    private CommonUtils commonUtils;

    @Mock
    private TenantSettingsService tenantSettingsService;

    private PdfContentByte dc;
    private BaseFont font;
    private Rectangle realPageSize;
    private Rectangle rect;
    private PdfReader reader;
    private PdfStamper stamper;
    private ByteArrayOutputStream outputStream;
    private byte[] pdfBytes;


    @BeforeEach
    void setUp() throws DocumentException, IOException {
        dc = mock(PdfContentByte.class);
        font = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.WINANSI, BaseFont.EMBEDDED);
        realPageSize = new Rectangle(0, 0, 595, 842); // A4 size
        rect = new Rectangle(100, 100, 500, 742);
        reader = mock(PdfReader.class);
        stamper = mock(PdfStamper.class);
        outputStream = new ByteArrayOutputStream();
        pdfBytes = new byte[0];

        MockitoAnnotations.initMocks(this);
        commonUtils.syncExecutorService = syncExecutorService;
        commonUtils.shipmentSettingsDao = shipmentSettingsDao;
    }

    @Test
    void constructCriteria_ValidCriteria_ReturnsFilterCriteria() {
        String fieldName = "testField";
        Object value = "testValue";
        String operator = "equals";
        String logicalOperator = "and";

        FilterCriteria filterCriteria = CommonUtils.constructCriteria(fieldName, value, operator, logicalOperator);

        assertNotNull(filterCriteria);
        assertNotNull(filterCriteria.getCriteria());
        assertNotNull(filterCriteria.getLogicOperator());
    }

    @Test
    void generateEAN13BarcodeImage_ValidBarcode_ReturnsBufferedImage() {
        String barcodeText = "123456789012";

        BufferedImage barcodeImage = CommonUtils.generateEAN13BarcodeImage(barcodeText, 200);

        assertNotNull(barcodeImage);
    }

    @Test
    void generateBarcodeImage_ValidBarcode_ReturnsByteArray() throws Exception {
        String barcodeText = "123456789";

        byte[] barcodeByteArray = CommonUtils.generateBarcodeImage(barcodeText);

        assertNotNull(barcodeByteArray);
    }

    @Test
    void constructor_WithObjectMapper_SuccessfullyInitializesMapper() {
        assertNotNull(commonUtils);
    }

    @Test
    void constructListCommonRequest_ValidInput_ReturnsListCommonRequest() {
        String fieldName = "testField";
        Object value = "testValue";
        String operator = "equals";

        ListCommonRequest result = CommonUtils.constructListCommonRequest(fieldName, value, operator);

        assertNotNull(result);
        assertEquals(1, result.getPageNo());
        assertEquals(Integer.MAX_VALUE, result.getPageSize());
        assertNotNull(result.getFilterCriteria());
        assertEquals(1, result.getFilterCriteria().size());
        FilterCriteria filterCriteria = result.getFilterCriteria().get(0);
        assertNotNull(filterCriteria);
        assertNotNull(filterCriteria.getInnerFilter());
        assertEquals(1, filterCriteria.getInnerFilter().size());
        FilterCriteria innerFilter = filterCriteria.getInnerFilter().get(0);
        assertNotNull(innerFilter);
        assertNotNull(innerFilter.getCriteria());
        assertEquals(fieldName, innerFilter.getCriteria().getFieldName());
        assertEquals(value, innerFilter.getCriteria().getValue());
        assertEquals(operator, innerFilter.getCriteria().getOperator());
    }

    @Test
    void constructListRequestFromEntityId_ValidInput_ReturnsListCommonRequest() {
        Long entityId = 123L;
        String entityType = "testType";

        ListCommonRequest result = CommonUtils.constructListRequestFromEntityId(entityId, entityType);

        assertNotNull(result);
        assertEquals(1, result.getPageNo());
        assertEquals(Integer.MAX_VALUE, result.getPageSize());
        assertNotNull(result.getFilterCriteria());
        assertEquals(1, result.getFilterCriteria().size());
        FilterCriteria entityIdCriteria = result.getFilterCriteria().get(0);
        assertNotNull(entityIdCriteria);
        assertNotNull(entityIdCriteria.getInnerFilter());
        assertEquals(2, entityIdCriteria.getInnerFilter().size());
        FilterCriteria entityIdFilter = entityIdCriteria.getInnerFilter().get(0);
        assertNotNull(entityIdFilter);
        assertNotNull(entityIdFilter.getCriteria());
        assertEquals("entityId", entityIdFilter.getCriteria().getFieldName());
        assertEquals(entityId, entityIdFilter.getCriteria().getValue());
        assertEquals("=", entityIdFilter.getCriteria().getOperator());
        FilterCriteria entityTypeFilter = entityIdCriteria.getInnerFilter().get(1);
        assertNotNull(entityTypeFilter);
        assertNotNull(entityTypeFilter.getCriteria());
        assertEquals("entityType", entityTypeFilter.getCriteria().getFieldName());
        assertEquals(entityType, entityTypeFilter.getCriteria().getValue());
        assertEquals("=", entityTypeFilter.getCriteria().getOperator());
    }


    @Test
    void getFilterCriteria_ValidInput_ReturnsCriteria() {
        String fieldName = "testField";
        Object value = "testValue";
        String operator = "equals";

        Criteria result = CommonUtils.getFilterCriteria(fieldName, value, operator);

        assertNotNull(result);
        assertEquals(fieldName, result.getFieldName());
        assertEquals(value, result.getValue());
        assertEquals(operator, result.getOperator());
    }

    @Test
    void andCriteria_WithNonNullRequest_ReturnsModifiedRequest() {
        ListCommonRequest request = andCriteria(Constants.SHIPMENT_ID, List.of(1L, 2L), "IN", null);
        var result = andCriteria(Constants.CONTAINER_ID, "", "ISNULL", request);

        assertNotNull(result);
        assertEquals(1, result.getPageNo());
        assertEquals(Integer.MAX_VALUE, result.getPageSize());
        assertNotNull(result.getFilterCriteria());
        assertEquals(1, result.getFilterCriteria().size());
        FilterCriteria filterCriteria = result.getFilterCriteria().get(0);
        assertNotNull(filterCriteria);
        assertNotNull(filterCriteria.getInnerFilter());
        assertEquals(2, filterCriteria.getInnerFilter().size());
    }

    @Test
    void orCriteria_test() {
        ListCommonRequest request = CommonUtils.orCriteria(Constants.SHIPMENT_ID, List.of(1L, 2L), "IN", null);
        var result = CommonUtils.orCriteria(Constants.CONTAINER_ID, "", "ISNULL", request);

        assertNotNull(result);
        assertEquals(1, result.getPageNo());
        assertEquals(Integer.MAX_VALUE, result.getPageSize());
        assertNotNull(result.getFilterCriteria());
        assertEquals(1, result.getFilterCriteria().size());
        FilterCriteria filterCriteria = result.getFilterCriteria().get(0);
        assertNotNull(filterCriteria);
        assertNotNull(filterCriteria.getInnerFilter());
        assertEquals(2, filterCriteria.getInnerFilter().size());
    }


    @Test
    void getConstrainViolationErrorMessage_ValidInput_ReturnsErrorMessage() {
        ConstraintViolation<?> constraintViolation = mock(ConstraintViolation.class);
        when(constraintViolation.getInvalidValue()).thenReturn("invalid value");
        when(constraintViolation.getMessage()).thenReturn("error message");

        Set<ConstraintViolation<?>> set = new HashSet<>();
        set.add(constraintViolation);

        Exception e = new ConstraintViolationException("constraint violation", set);

        String errorMessage = CommonUtils.getConstrainViolationErrorMessage(e);

        assertEquals("[invalid value : error message]", errorMessage);
    }


    @Test
    void inWords_ValidInput_ReturnsOverflow() {
        Long num = 9999999999L;

        String words = CommonUtils.inWords(num);

        assertEquals("overflow", words);
    }

    @Test
    void emptyIfNull_NullIterable_ReturnsEmptyList() {
        Iterable<Integer> iterable = null;

        Iterable<Integer> result = CommonUtils.emptyIfNull(iterable);

        assertEquals(Collections.emptyList(), result);
    }

    @Test
    void emptyIfNull_NonNullIterable_ReturnsSameIterable() {
        List<Integer> list = Arrays.asList(1, 2, 3);
        Iterable<Integer> result = CommonUtils.emptyIfNull(list);
        assertEquals(list, result);
    }


    @Test
    void testAddWaterMark() {
        CommonUtils.AddWaterMark(dc, "Test Watermark", font, 50, 35, new BaseColor(70, 70, 255), realPageSize, rect);

        InOrder inOrder = inOrder(dc);
        inOrder.verify(dc).saveState();
        inOrder.verify(dc).setGState(any(PdfGState.class));
        inOrder.verify(dc).setColorFill(new BaseColor(70, 70, 255));
        inOrder.verify(dc).beginText();
        inOrder.verify(dc).setFontAndSize(font, 50);
        inOrder.verify(dc).showTextAligned(Element.ALIGN_CENTER, "Test Watermark", 300f, 421f, 35);
        inOrder.verify(dc).endText();
        inOrder.verify(dc).restoreState();
    }

    private byte[] createSamplePdf() throws DocumentException, IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Document document = new Document();
        PdfWriter.getInstance(document, baos);
        document.open();
        document.add(new Paragraph("Sample PDF Content"));
        document.close();
        return baos.toByteArray();
    }


    @Test
    void addWatermarkToPdfBytes_InvalidInput_ThrowsInvalidPdfException() throws DocumentException, IOException {
        byte[] pdfBytes = new byte[1024]; // example byte array
        BaseFont bf = BaseFont.createFont();
        String watermark = "Watermark"; // example watermark text

        assertThrows(InvalidPdfException.class, () -> CommonUtils.addWatermarkToPdfBytes(pdfBytes, bf, watermark));
    }

    @Test
    void getByteResource_ValidInput_ReturnsByteArrayResource() throws IOException {
        String fileName = "example.pdf";
        InputStream inputStream = new ByteArrayInputStream(new byte[1024]); // example input stream

        ByteArrayResource result = CommonUtils.getByteResource(inputStream, fileName);

        assertNotNull(result);
        assertEquals(fileName, result.getFilename());
    }

    @Test
    void roundOffToTwoDecimalPlace_ValidInput_ReturnsRoundedValue() {
        double number = 3.14159;

        double result = CommonUtils.roundOffToTwoDecimalPlace(number);

        assertEquals(3.14, result);
    }

    @Test
    void stringValueOf_NullInput_ReturnsNull() {
        String result = CommonUtils.stringValueOf(null);
        assertNull(result);
    }

    @Test
    void stringValueOf_NonNullInput_ReturnsStringRepresentation() {
        Object object = 123;
        String result = CommonUtils.stringValueOf(object);
        assertNotNull(result);
        assertEquals("123", result);
    }

    @Test
    void IsStringNullOrEmpty_NullInput_ReturnsTrue() {
        boolean result = CommonUtils.IsStringNullOrEmpty(null);
        assertTrue(result);
    }

    @Test
    void IsStringNullOrEmpty_EmptyStringInput_ReturnsTrue() {
        boolean result = CommonUtils.IsStringNullOrEmpty("");
        assertTrue(result);
    }

    @Test
    void IsStringNullOrEmpty_NonEmptyStringInput_ReturnsFalse() {
        String input = "Hello";
        boolean result = CommonUtils.IsStringNullOrEmpty(input);
        assertFalse(result);
    }

    @Test
    void testConvertToClass() {
        Object obj = new Object();
        when(jsonHelper.convertValue(obj, String.class)).thenReturn("converted");

        String result = commonUtils.convertToClass(obj, String.class);

        assertEquals("converted", result);
    }


    @Test
    void testConvertToEntityList() {
        List<Object> lst = List.of(new Object(), new Object());

        List<MultiTenancy> result = commonUtils.convertToEntityList(lst, MultiTenancy.class);

        assertEquals(lst.size(), result.size());
    }

    @Test
    void testImageToByte() throws IOException {
        BufferedImage img = new BufferedImage(100, 100, BufferedImage.TYPE_INT_RGB);
        byte[] result = CommonUtils.ImageToByte(img);

        assertNotNull(result);
    }

    @Test
    void testHasUnsupportedCharacters() {
        String input = "ValidString123";
        boolean result = CommonUtils.HasUnsupportedCharacters(input);

        assertFalse(result);

        input = "InvalidString\u001F";
        result = CommonUtils.HasUnsupportedCharacters(input);

        assertTrue(result);
    }

    @Test
    void testConcatAndAddContent() throws DocumentException, IOException {
        List<byte[]> pdfByteContent = List.of(createSamplePdf(), createSamplePdf());
        byte[] result = CommonUtils.concatAndAddContent(pdfByteContent);

        assertNotNull(result);
    }

    @Test
    void testRemoveLastPage() throws IOException, DocumentException {
        byte[] pdfBytes = createSamplePdfWithMultiplePages();
        byte[] result = CommonUtils.removeLastPage(pdfBytes);

        assertNotNull(result);
    }

    @Test
    void testGetLastPage() throws IOException, DocumentException {
        byte[] pdfBytes = createSamplePdfWithMultiplePages();
        byte[] result = CommonUtils.getLastPage(pdfBytes);

        assertNotNull(result);
    }

    @Test
    void testGetErrorResponseMessage_WithTransactionSystemException() {
        Throwable rootCause = mock(Throwable.class);
        when(rootCause.getMessage()).thenReturn("Root cause message");

        TransactionSystemException transactionSystemException = new TransactionSystemException("Transaction failed");
        transactionSystemException.initCause(rootCause);

        String result = CommonUtils.getErrorResponseMessage(transactionSystemException, CommonUtilsTest.class);

        assertEquals("Root cause message", result);
    }

    @Test
    void testGetErrorResponseMessage_WithGenericException() {
        Exception genericException = new Exception("Generic exception message");
        String result = CommonUtils.getErrorResponseMessage(genericException, CommonUtilsTest.class);
        assertEquals("Generic exception message", result);
    }

    private byte[] createSamplePdfWithMultiplePages() throws DocumentException, IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Document document = new Document();
        PdfWriter.getInstance(document, baos);
        document.open();
        for (int i = 0; i < 3; i++) {
            document.newPage();
            document.add(new Paragraph("Sample PDF Content - Page " + (i + 1)));
        }
        document.close();
        return baos.toByteArray();
    }

    @Test
    void defaultShipmentSettings() {
        when(shipmentSettingsDao.getSettingsByTenantIdWithCache(any())).thenReturn(Optional.of(new ShipmentSettingsDetails()));
        assertNotNull(commonUtils.getShipmentSettingFromContext());
    }

    @Test
    void defaultShipmentSettingsWithValue() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        assertNotNull(commonUtils.getShipmentSettingFromContext());
    }

    @Test
    void defaultTenantSettings() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(null);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(new V1TenantSettingsResponse());
        assertNotNull(commonUtils.getCurrentTenantSettings());
    }

    @Test
    void defaultTenantSettingsWithValue() {
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        assertNotNull(commonUtils.getCurrentTenantSettings());
    }
}