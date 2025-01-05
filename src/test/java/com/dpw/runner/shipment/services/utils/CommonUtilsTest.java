package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.*;
import com.itextpdf.text.exceptions.InvalidPdfException;
import com.itextpdf.text.pdf.*;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.transaction.TransactionSystemException;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.awt.image.BufferedImage;
import java.io.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD_CAPS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.PermissionConstants.OCEAN_DG_APPROVER;
import static com.dpw.runner.shipment.services.commons.constants.PermissionConstants.OCEAN_DG_COMMERCIAL_APPROVER;
import static com.dpw.runner.shipment.services.entity.enums.OceanDGStatus.OCEAN_DG_REQUESTED;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class CommonUtilsTest {

    @Mock
    private INotificationService notificationService;

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

    @Mock
    private IV1Service iv1Service;

    @Mock
    private ConsolidationDetails consolidationDetails;

    @Mock
    private AchievedQuantities achievedQuantities;

    @Mock
    private Allocations allocations;

    @Mock
    private IAuditLogDao iAuditLogDao;

    @Mock
    private ShipmentService shipmentService;

    @Mock
    private ShipmentDetails shipmentDetails;

    @Mock
    private CarrierDetails carrierDetailsMock;

    @Mock
    private V1TenantSettingsResponse v1TenantSettingsResponse;

    @Mock
    private EmailTemplatesRequest emailTemplateModel;

    @Mock
    private ICarrierDetailsDao carrierDetailsDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    private ShipmentDao shipmentDao;

    @Mock
    private ConsolidationDao consolidationDetailsDao;

    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;


    private PdfContentByte dc;
    private BaseFont font;
    private Rectangle realPageSize;
    private Rectangle rect;
    private PdfReader reader;
    private PdfStamper stamper;
    private ByteArrayOutputStream outputStream;
    private PrintStream originalOut;
    private byte[] pdfBytes;

    @AfterEach
    void tearDown() {
        commonUtils.syncExecutorService.shutdown();
    }

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
        commonUtils.syncExecutorService = Executors.newFixedThreadPool(2);
        commonUtils.shipmentSettingsDao = shipmentSettingsDao;

        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        mockUser.setEmail("email");
        mockUser.setCode("TEST_CODE");
        mockUser.setUsername("TestUser");
        mockUser.setTenantDisplayName("Test Tenant");
        UserContext.setUser(mockUser);
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
    void testAddBlankPage() throws IOException, DocumentException {
        int originalDocumentPageCount = 2;
        byte[] originalPdf = createSamplePdf(originalDocumentPageCount);
        byte[] updatedPdf = CommonUtils.addBlankPage(originalPdf);

        PdfReader pdfReader = new PdfReader(updatedPdf);
        int pageCount = pdfReader.getNumberOfPages();
        pdfReader.close();

        // Assert that the updated PDF now has 3 pages
        assertEquals(3, pageCount, "The PDF should have 3 pages after adding a blank page.");
    }

    private byte[] createSamplePdf(int pageCount) throws DocumentException {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        Document document = new Document();
        PdfWriter.getInstance(document, byteArrayOutputStream);

        document.open();
        for (int i = 0; i < pageCount; i++) {
            document.newPage();
            document.add(new com.itextpdf.text.Paragraph("Page " + (i + 1)));
        }
        document.close();

        return byteArrayOutputStream.toByteArray();
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

    @Test
    void testSetInterBranchContextForHub_withBothFlagsEnabled() {
        V1TenantSettingsResponse mockTenantSettingsResponse = mock(V1TenantSettingsResponse.class);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(mockTenantSettingsResponse);
        when(mockTenantSettingsResponse.getIsMAWBColoadingEnabled()).thenReturn(true);
        when(mockTenantSettingsResponse.getIsColoadingMAWBStationEnabled()).thenReturn(true);
        when(mockTenantSettingsResponse.getColoadingBranchIds()).thenReturn(List.of(1,2));

        assertNotNull(mockTenantSettingsResponse.getColoadingBranchIds());

        InterBranchDto interBranchDto = new InterBranchDto();
        interBranchDto.setColoadStationsTenantIds(tenantSettingsService.getV1TenantSettings(any()).getColoadingBranchIds());
        interBranchDto.setHub(true);

        commonUtils.setInterBranchContextForHub();
        InterBranchDto interBranchContext = InterBranchContext.getContext();
        assertNotNull(interBranchContext);
        assertTrue(interBranchContext.isHub());
        assertEquals(List.of(1,2), interBranchContext.getColoadStationsTenantIds());
    }

    @Test
    void testSetInterBranchContextForHub_withMAWBColoadingDisabled() {
        V1TenantSettingsResponse mockTenantSettingsResponse = mock(V1TenantSettingsResponse.class);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(mockTenantSettingsResponse);
        when(mockTenantSettingsResponse.getIsMAWBColoadingEnabled()).thenReturn(false);
        assertTrue(mockTenantSettingsResponse.getColoadingBranchIds().isEmpty());

        commonUtils.setInterBranchContextForHub();

        InterBranchDto interBranchContext = InterBranchContext.getContext();
        assertNotNull(interBranchContext);
        assertFalse(interBranchContext.isHub());
        assertNull(interBranchContext.getColoadStationsTenantIds());
    }

    @Test
    void testSetInterBranchContextForHub_withColoadingMAWBStationDisabled() {
        V1TenantSettingsResponse mockTenantSettingsResponse = mock(V1TenantSettingsResponse.class);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(mockTenantSettingsResponse);
        when(mockTenantSettingsResponse.getIsMAWBColoadingEnabled()).thenReturn(true);
        when(mockTenantSettingsResponse.getIsColoadingMAWBStationEnabled()).thenReturn(false);
        assertTrue(mockTenantSettingsResponse.getColoadingBranchIds().isEmpty());

        commonUtils.setInterBranchContextForHub();

        InterBranchDto interBranchContext = InterBranchContext.getContext();
        assertNotNull(interBranchContext);
        assertFalse(interBranchContext.isHub());
        assertNull(interBranchContext.getColoadStationsTenantIds());
    }

    @Test
    void testSetInterBranchContextForColoadStation_withColoadingEnabled() {
        V1TenantSettingsResponse mockTenantSettingsResponse = mock(V1TenantSettingsResponse.class);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(mockTenantSettingsResponse);
        when(mockTenantSettingsResponse.getIsMAWBColoadingEnabled()).thenReturn(true);
        when(mockTenantSettingsResponse.getColoadingBranchIds()).thenReturn(List.of(1,2));

        List<CoLoadingMAWBDetailsResponse> mockDetails = Arrays.asList(
                new CoLoadingMAWBDetailsResponse(1L, 1, 1),
                new CoLoadingMAWBDetailsResponse(2L, 2, 2)
        );

        V1DataResponse v1DataResponse = mock(V1DataResponse.class);
        when(iv1Service.getCoLoadingStations(any())).thenReturn(v1DataResponse);
        when(commonUtils.fetchColoadingDetails()).thenReturn(mockDetails);

        commonUtils.setInterBranchContextForColoadStation();
        InterBranchDto context = InterBranchContext.getContext();
        assert context != null;
        assertNotNull(context.getHubTenantIds());
        assertTrue(context.isCoLoadStation());
    }

    @Test
    void testSetInterBranchContextForColoadStation_withColoadingDisabled() {
        V1TenantSettingsResponse mockTenantSettingsResponse = mock(V1TenantSettingsResponse.class);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(mockTenantSettingsResponse);
        when(mockTenantSettingsResponse.getIsMAWBColoadingEnabled()).thenReturn(false);

        commonUtils.setInterBranchContextForColoadStation();
        assertNotNull(InterBranchContext.getContext());
    }

    @Test
    void testSetInterBranchContextForColoadStation_withNullColoadingBranchIds() {
        V1TenantSettingsResponse mockTenantSettingsResponse = mock(V1TenantSettingsResponse.class);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(mockTenantSettingsResponse);
        when(mockTenantSettingsResponse.getIsMAWBColoadingEnabled()).thenReturn(true);
        when(mockTenantSettingsResponse.getColoadingBranchIds()).thenReturn(null);

        commonUtils.setInterBranchContextForColoadStation();
        assertNotNull(InterBranchContext.getContext());
    }

    @Test
    void testUpdateConsolOpenForAttachment_withNullAchievedQuantities() {
        when(consolidationDetails.getAchievedQuantities()).thenReturn(null);
        commonUtils.updateConsolOpenForAttachment(consolidationDetails);
        verify(consolidationDetails, never()).setOpenForAttachment(anyBoolean());
    }

    @Test
    void testUpdateConsolOpenForAttachment_withWeightUtilizationAbove100() {
        when(consolidationDetails.getTransportMode()).thenReturn("AIR");
        when(consolidationDetails.getAchievedQuantities()).thenReturn(achievedQuantities);
        when(achievedQuantities.getWeightUtilization()).thenReturn("150");
        when(achievedQuantities.getVolumeUtilization()).thenReturn("50");
        commonUtils.updateConsolOpenForAttachment(consolidationDetails);
        verify(consolidationDetails, times(1)).setOpenForAttachment(false);
    }

    @Test
    void testUpdateConsolOpenForAttachment_withVolumeUtilizationAbove100() {
        when(consolidationDetails.getTransportMode()).thenReturn("AIR");
        when(consolidationDetails.getAchievedQuantities()).thenReturn(achievedQuantities);
        when(achievedQuantities.getWeightUtilization()).thenReturn("50");
        when(achievedQuantities.getVolumeUtilization()).thenReturn("150");
        commonUtils.updateConsolOpenForAttachment(consolidationDetails);
        verify(consolidationDetails, times(1)).setOpenForAttachment(false);
    }

    @Test
    void testUpdateConsolOpenForAttachment_AchievedQuantitiesIsNull() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setAchievedQuantities(null);

        commonUtils.updateConsolOpenForAttachment(details);

        assertNull(details.getOpenForAttachment());
    }

    @Test
    void testUpdateConsolOpenForAttachment_WeightUtilizationAndVolumeUtilizationNull() {
        achievedQuantities = new AchievedQuantities();
        ConsolidationDetails details = new ConsolidationDetails();
        details.setAchievedQuantities(achievedQuantities);

        commonUtils.updateConsolOpenForAttachment(details);

        assertNull(details.getOpenForAttachment());
    }

    @Test
    void testCalculateConsolUtilization_NullAllocations() throws RunnerException {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setAllocations(null);
        details.setAchievedQuantities(new AchievedQuantities());

        ConsolidationDetails result = commonUtils.calculateConsolUtilization(details);

        assertThat(result.getAllocations()).isNotNull();
    }

    @Test
    void testCalculateConsolUtilization_NullAchievedQuantities() throws RunnerException {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setAllocations(new Allocations());
        details.setAchievedQuantities(null);

        ConsolidationDetails result = commonUtils.calculateConsolUtilization(details);

        assertThat(result.getAchievedQuantities()).isNotNull();
    }


    @Test
    void testCalculateConsolUtilization_WeightAndVolumeUtilization() throws RunnerException {
        ConsolidationDetails details = getConsolidationDetails();

        try (MockedStatic<UnitConversionUtility> mockedStatic = Mockito.mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(
                            eq(Constants.MASS), any(BigDecimal.class), eq("KG"), eq(Constants.WEIGHT_UNIT_KG)))
                    .thenReturn(new BigDecimal("100"));

            mockedStatic.when(() -> UnitConversionUtility.convertUnit(
                            eq(Constants.VOLUME), any(BigDecimal.class), eq("M3"), eq(Constants.VOLUME_UNIT_M3)))
                    .thenReturn(new BigDecimal("50"));

            ConsolidationDetails result = commonUtils.calculateConsolUtilization(details);

            assertThat(result.getAchievedQuantities().getConsolidatedWeightUnit())
                    .isNotNull()
                    .isEqualTo("KG");

            assertThat(result.getAllocations().getWeightUnit())
                    .isNotNull()
                    .isEqualTo("KG");

            assertThat(result.getAchievedQuantities().getWeightUtilization()).isEqualTo("100.0");
            assertThat(result.getAchievedQuantities().getVolumeUtilization()).isEqualTo("100.0");
        }
    }

    private static @NotNull ConsolidationDetails getConsolidationDetails() {
        ConsolidationDetails details = new ConsolidationDetails();
        AchievedQuantities achievedQuantities = new AchievedQuantities();
        Allocations allocations = new Allocations();

        achievedQuantities.setConsolidatedWeightUnit("KG");
        achievedQuantities.setConsolidatedWeight(new BigDecimal("100"));
        achievedQuantities.setConsolidatedVolumeUnit("M3");
        achievedQuantities.setConsolidatedVolume(new BigDecimal("50"));

        allocations.setWeightUnit("KG");
        allocations.setWeight(new BigDecimal("200"));
        allocations.setVolumeUnit("M3");
        allocations.setVolume(new BigDecimal("100"));

        details.setAchievedQuantities(achievedQuantities);
        details.setAllocations(allocations);
        return details;
    }

    @Test
    void testCalculateConsolUtilization_ExceptionHandling() {
        ConsolidationDetails details = getDetails();

        try (MockedStatic<UnitConversionUtility> mockedStatic = Mockito.mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(
                            anyString(), any(BigDecimal.class), anyString(), anyString()))
                    .thenThrow(new RunnerException("Conversion error"));

            assertThatThrownBy(() -> commonUtils.calculateConsolUtilization(details))
                    .isInstanceOf(RunnerException.class)
                    .hasMessageContaining("Conversion error");
        }
    }

    @Test
    void testCalculateConsolUtilization_ExceptionWithNullMessage() {
        ConsolidationDetails details = new ConsolidationDetails();
        achievedQuantities = new AchievedQuantities();
        allocations = new Allocations();

        details.setAchievedQuantities(achievedQuantities);
        details.setAllocations(allocations);

        try (MockedStatic<UnitConversionUtility> mockedStatic = mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(anyString(), any(BigDecimal.class), anyString(), anyString()))
                    .thenThrow(new RuntimeException());

            try {
                commonUtils.calculateConsolUtilization(details);
            } catch (RunnerException e) {
                assertThat(e.getMessage()).isEqualTo(DaoConstants.DAO_CALCULATION_ERROR);
            }
        }
    }

    private static @NotNull ConsolidationDetails getDetails() {
        ConsolidationDetails details = new ConsolidationDetails();
        AchievedQuantities achievedQuantities = new AchievedQuantities();
        Allocations allocations = new Allocations();

        achievedQuantities.setConsolidatedWeightUnit("KG");
        achievedQuantities.setConsolidatedWeight(new BigDecimal("100"));

        allocations.setWeightUnit("KG");
        allocations.setWeight(new BigDecimal("200"));

        details.setAchievedQuantities(achievedQuantities);
        details.setAllocations(allocations);
        return details;
    }

    @Test
    void testCalculateConsolUtilization_WeightUtilizationZero() throws RunnerException {
        ConsolidationDetails details = new ConsolidationDetails();
        achievedQuantities = new AchievedQuantities();
        allocations = new Allocations();

        achievedQuantities.setConsolidatedWeightUnit("KG");
        achievedQuantities.setConsolidatedWeight(new BigDecimal("100"));
        allocations.setWeightUnit("KG");
        allocations.setWeight(BigDecimal.ZERO);

        details.setAchievedQuantities(achievedQuantities);
        details.setAllocations(allocations);

        try (MockedStatic<UnitConversionUtility> mockedStatic = mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(anyString(), any(BigDecimal.class), anyString(), anyString()))
                    .thenReturn(new BigDecimal("0"));

            ConsolidationDetails result = commonUtils.calculateConsolUtilization(details);

            assertThat(result.getAchievedQuantities().getWeightUtilization()).isEqualTo("0");
        }
    }

    @Test
    void testCalculateConsolUtilization_VolumeUtilizationZero() throws RunnerException {
        ConsolidationDetails details = new ConsolidationDetails();
        achievedQuantities = new AchievedQuantities();
        allocations = new Allocations();

        achievedQuantities.setConsolidatedVolumeUnit("M3");
        achievedQuantities.setConsolidatedVolume(new BigDecimal("0"));
        allocations.setVolumeUnit("M3");
        allocations.setVolume(BigDecimal.ZERO);

        details.setAchievedQuantities(achievedQuantities);
        details.setAllocations(allocations);

        try (MockedStatic<UnitConversionUtility> mockedStatic = mockStatic(UnitConversionUtility.class)) {
            mockedStatic.when(() -> UnitConversionUtility.convertUnit(anyString(), any(BigDecimal.class), anyString(), anyString()))
                    .thenReturn(new BigDecimal("0"));

            ConsolidationDetails result = commonUtils.calculateConsolUtilization(details);

            assertThat(result.getAchievedQuantities().getVolumeUtilization()).isEqualTo("0");
        }
    }

    @Test
    void testUpdateUnLocData() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        Map<String, EntityTransferUnLocations> unlocationsMap = new HashMap<>();
        unlocationsMap.put("test", new EntityTransferUnLocations());
        when(masterDataUtils.getLocationDataFromCache(any(), any())).thenReturn(unlocationsMap);
        commonUtils.updateUnLocData(carrierDetails, null);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateUnLocData_Data1() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOrigin("test1");
        oldCarrierDetails.setOriginPort("test");
        oldCarrierDetails.setDestination("test");
        oldCarrierDetails.setDestinationPort("test");
        commonUtils.updateUnLocData(carrierDetails, oldCarrierDetails);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateUnLocData_Data2() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOrigin("test");
        oldCarrierDetails.setOriginPort("test1");
        oldCarrierDetails.setDestination("test");
        oldCarrierDetails.setDestinationPort("test");
        commonUtils.updateUnLocData(carrierDetails, oldCarrierDetails);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateUnLocData_Data3() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOrigin("test");
        oldCarrierDetails.setOriginPort("test");
        oldCarrierDetails.setDestination("test1");
        oldCarrierDetails.setDestinationPort("test");
        commonUtils.updateUnLocData(carrierDetails, oldCarrierDetails);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateUnLocData_Data4() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOrigin("test");
        oldCarrierDetails.setOriginPort("test");
        oldCarrierDetails.setDestination("test");
        oldCarrierDetails.setDestinationPort("test1");
        commonUtils.updateUnLocData(carrierDetails, oldCarrierDetails);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateUnLocData_Data5() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        CarrierDetails oldCarrierDetails = new CarrierDetails();
        oldCarrierDetails.setOrigin("test");
        oldCarrierDetails.setOriginPort("test");
        oldCarrierDetails.setDestination("test");
        oldCarrierDetails.setDestinationPort("test");
        commonUtils.updateUnLocData(carrierDetails, oldCarrierDetails);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateUnLocData1() {
        CarrierDetails carrierDetails = new CarrierDetails();
        commonUtils.updateUnLocData(carrierDetails, null);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void sendEmailForPullPushRequestStatusWithoutEmailTemplate() throws Exception {
        Set<ShipmentRequestedType> shipmentRequestedTypes = new HashSet<>();
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder().build(),
                ConsolidationDetails.builder().build(),
                SHIPMENT_PULL_REQUESTED,
                "rejectRemarks",
                new HashMap<>(),
                shipmentRequestedTypes,
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                "username", null);
        assertFalse(shipmentRequestedTypes.isEmpty());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullAcceptedNoTemplate() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_ACCEPTED,
                "rejectRemarks",
                new HashMap<>(),
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullAccepted() throws Exception {
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_ACCEPTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_ACCEPTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullAcceptedCases() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PULL_ACCEPTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_ACCEPTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("createdConsole", "createdConsole@gmail.com");
                    put("assigned", "assigned@gmail.com");
                    put("createdShipment", "createdShipment@gmail.com");
                    put("username", "username@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullAcceptedCases2() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PULL_ACCEPTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_ACCEPTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullReject() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_REJECTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_REJECTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullRejectNoTemplate() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_REJECTED,
                "rejectRemarks",
                new HashMap<>(),
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullRejectCases() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PULL_REJECTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_REJECTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("createdConsole", "createdConsole@gmail.com");
                    put("assigned", "assigned@gmail.com");
                    put("createdShipment", "createdShipment@gmail.com");
                    put("username", "username@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullRejectCases2() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PULL_REJECTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_REJECTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushWithdrawRequest() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .consolidationNumber("1")
                .sourceTenantId(1L)
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setTenantId(1);
        consolidationDetails1.setCreatedBy("CreatedBy");
        consolidationDetails1.setId(1L);
        HashMap tenantModelMap = new HashMap<>();
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Code");
        tenantModel.setTenantName("TenantName");
        tenantModelMap.put(1,tenantModel);
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .shipmentId("2")
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                consolidationDetails1,
                SHIPMENT_PUSH_WITHDRAW,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_WITHDRAW, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, tenantModelMap);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPullWithdrawRequest() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .consolidationNumber("1")
                .sourceTenantId(1L)
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setTenantId(1);
        consolidationDetails1.setCreatedBy("CreatedBy");
        consolidationDetails1.setId(1L);
        HashMap tenantModelMap = new HashMap<>();
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Code");
        tenantModel.setTenantName("TenantName");
        tenantModelMap.put(1,tenantModel);
        tenantModelMap.put(null, tenantModel);
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .shipmentId("2")
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                consolidationDetails1,
                SHIPMENT_PULL_WITHDRAW,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_WITHDRAW, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, tenantModelMap);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRequest() throws Exception {
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PUSH_REQUESTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_REQUESTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRequestNoTemplate() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PUSH_REQUESTED,
                "rejectRemarks",
                new HashMap<>(),
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRequestCases() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PUSH_REQUESTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_REQUESTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("createdConsole", "createdConsole@gmail.com");
                    put("assigned", "assigned@gmail.com");
                    put("createdShipment", "createdShipment@gmail.com");
                    put("username", "username@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRequestCases2() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PUSH_REQUESTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_REQUESTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushAccept() throws Exception {
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PUSH_ACCEPTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_ACCEPTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushAcceptNoTemplate() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PUSH_ACCEPTED,
                "rejectRemarks",
                new HashMap<>(),
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushAcceptCases() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PUSH_ACCEPTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_ACCEPTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("createdConsole", "createdConsole@gmail.com");
                    put("assigned", "assigned@gmail.com");
                    put("createdShipment", "createdShipment@gmail.com");
                    put("username", "username@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushAcceptCases2() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PUSH_ACCEPTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_ACCEPTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushReject() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PUSH_REJECTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_REJECTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRejectNoTemplate() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PUSH_REJECTED,
                "rejectRemarks",
                new HashMap<>(),
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRejectCases() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PUSH_REJECTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_REJECTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("createdConsole", "createdConsole@gmail.com");
                    put("assigned", "assigned@gmail.com");
                    put("createdShipment", "createdShipment@gmail.com");
                    put("username", "username@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusPushRejectCases2() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_PUSH_REJECTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PUSH_REJECTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatus() throws Exception {
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_REQUESTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_REQUESTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatus_Cases() throws Exception {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .assignedTo("assigned")
                .carrierDetails(CarrierDetails.builder().build())
                .build();
        shipmentDetails.setCreatedBy("created");
        shipmentDetails.setTenantId(56);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_REQUESTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_REQUESTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("assigned", "assigned@gmail.com");
                    put("created", "created@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatus_Cases2() throws Exception {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .assignedTo("assigned")
                .carrierDetails(CarrierDetails.builder().build())
                .build();
        shipmentDetails.setCreatedBy("created");
        shipmentDetails.setTenantId(56);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        UserContext.getUser().setEmail(null);
        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(V1TenantSettingsResponse.builder().build());
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_PULL_REQUESTED,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_PULL_REQUESTED, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void getEmailTemplate() {
        List<EmailTemplatesRequest> emailTemplatesRequests = new ArrayList<>();
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_PULL_REQUESTED_EMAIL_TYPE).build());
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_PULL_ACCEPTED_EMAIL_TYPE).build());
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_PULL_REJECTED_EMAIL_TYPE).build());
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_PUSH_REQUESTED_EMAIL_TYPE).build());
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_PUSH_ACCEPTED_EMAIL_TYPE).build());
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_PUSH_REJECTED_EMAIL_TYPE).build());
        emailTemplatesRequests.add(EmailTemplatesRequest.builder().type(SHIPMENT_DETACH_EMAIL_TYPE).build());
        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().entities(emailTemplatesRequests).build());
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(emailTemplatesRequests);
        Map<ShipmentRequestedType, EmailTemplatesRequest> response = new HashMap<>();
        commonUtils.getEmailTemplate(response);
        assertEquals(7, response.size());
    }

    @Test
    void getToAndCCEmailIds() {
        Set<Integer> tenantIds = new HashSet<>();
        tenantIds.add(1);
        Map<Integer, V1TenantSettingsResponse> response = new HashMap<>();
        commonUtils.getToAndCCEmailIdsFromTenantSettings(tenantIds, response);
        assertEquals(0, response.size());
    }

    @Test
    void getUserDetails() {
        Set<String> usernamesList = new HashSet<>();
        Map<String, String> usernameEmailsMap = new HashMap<>();
        List<UsersDto> usersDtos = new ArrayList<>();
        usersDtos.add(UsersDto.builder().Username("username").Email("email").build());
        when(iv1Service.getUserDetails(any())).thenReturn(V1DataResponse.builder().entities(usersDtos).build());
        when(jsonHelper.convertValueToList(any(), eq(UsersDto.class))).thenReturn(usersDtos);
        commonUtils.getUserDetails(usernamesList, usernameEmailsMap);
        assertFalse(usernameEmailsMap.isEmpty());
    }

    @Test
    void getUnLocationsData() {
        Map<String, UnlocationsResponse> map = new HashMap<>();
        commonUtils.getUnLocationsData(null, map);
        assertTrue(map.isEmpty());
    }

    @Test
    void getUnLocationsData_Empty() {
        Map<String, UnlocationsResponse> map = new HashMap<>();
        commonUtils.getUnLocationsData(new ArrayList<>(), map);
        assertTrue(map.isEmpty());
    }

    @Test
    void getUnLocationsData_Value() {
        Map<String, UnlocationsResponse> map = new HashMap<>();
        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>() {{ put("unloc", UnlocationsResponse.builder().build()); }});
        commonUtils.getUnLocationsData(List.of("unloc"), map);
        assertFalse(map.isEmpty());
    }

    @Test
    void getCarriersData() {
        Map<String, CarrierMasterData> map = new HashMap<>();
        commonUtils.getCarriersData(null, map);
        assertTrue(map.isEmpty());
    }

    @Test
    void getCarriersData_Empty() {
        Map<String, CarrierMasterData> map = new HashMap<>();
        commonUtils.getCarriersData(new ArrayList<>(), map);
        assertTrue(map.isEmpty());
    }

    @Test
    void getCarriersData_Value() {
        Map<String, CarrierMasterData> map = new HashMap<>();
        when(masterDataUtils.getCarriersData(any())).thenReturn(new HashMap<>() {{ put("carrier", CarrierMasterData.builder().build()); }});
        commonUtils.getCarriersData(List.of("carrier"), map);
        assertFalse(map.isEmpty());
    }

    @Test
    void testSendRejectionEmailsExplicitly() {
        CommonUtils spyService = spy(commonUtils);
        when(masterDataUtils.withMdc(any())).thenReturn(mockRunnable());
        spyService.sendRejectionEmailsExplicitly(List.of(ShipmentDetails.builder().build()), List.of(ConsoleShipmentMapping.builder().build()),
                new HashSet<>(), List.of(ConsolidationDetails.builder().build()));
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void testGetTenantSettings() {
        Map<Integer, Object> response = commonUtils.getTenantSettings(new ArrayList<>());
        assertTrue(response.isEmpty());
    }

    @Test
    void testGetTenantSettings1() {
        when(iv1Service.getTenantDetails(any()))
                .thenReturn(TenantDetailsByListResponse.builder()
                        .entities(new ArrayList<>(List.of(TenantDetailsByListResponse.TenantDetails.builder().tenantId(2).build()))).build());
        Map<Integer, Object> response = commonUtils.getTenantSettings(List.of(2));
        assertFalse(response.isEmpty());
    }

    @Test
    void testChangeShipmentDGStatusToReqd1() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, true);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED).build(), true);
        assertTrue(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd2() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, false);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED).build(), true);
        assertTrue(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd3() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, true);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED).build(), true);
        assertFalse(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd4() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, false);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_APPROVAL_REQUIRED).build(), true);
        assertTrue(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd5() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, true);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED).build(), true);
        assertFalse(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd6() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, false);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_REJECTED).build(), true);
        assertTrue(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd7() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, true);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED).build(), true);
        assertTrue(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd8() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, false);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED).build(), true);
        assertTrue(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd9() {
        UserContext.getUser().getPermissions().put(OCEAN_DG_APPROVER, true);
        UserContext.getUser().getPermissions().put(OCEAN_DG_COMMERCIAL_APPROVER, true);
        boolean response = commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().oceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED).build(), true);
        assertFalse(response);
    }

    @Test
    void testCheckIfDGClass1() {
        boolean response = commonUtils.checkIfDGClass1("1.1");
        assertTrue(response);
    }

    @Test
    void testCheckIfDGClass2() {
        boolean response = commonUtils.checkIfDGClass1("2");
        assertFalse(response);
    }

    @Test
    void testCheckIfDGClass3() {
        boolean response = commonUtils.checkIfDGClass1(null);
        assertFalse(response);
    }

    @Test
    void testCheckIfAnyDGClass() throws RunnerException {
        boolean response = commonUtils.checkIfAnyDGClass(null);
        assertFalse(response);
    }

    @Test
    void testCheckIfAnyDGClass2() throws RunnerException {
        boolean response = commonUtils.checkIfAnyDGClass("1.1");
        assertTrue(response);
    }

    @Test
    void testCheckIfAnyDGClass3() throws RunnerException {
        assertThrows(RunnerException.class, () -> commonUtils.checkIfAnyDGClass("7.1"));
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertFalse(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking1() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setHazardous(true);
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking2() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setDGClass("2.1");
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking3() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setUnNumber("un");
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking4() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setProperShippingName("psp");
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking5() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setPackingGroup("pg");
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking6() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setMinimumFlashPointUnit("CEL");
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking7() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setMinimumFlashPoint(BigDecimal.ONE);
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInPacking8() {
        Packing packing = new Packing();
        PackingRequest packingRequest = new PackingRequest();
        packingRequest.setMarinePollutant(true);
        boolean response = commonUtils.checkIfDGFieldsChangedInPacking(packingRequest, packing);
        assertTrue(response);
    }

    private Runnable mockRunnable() {
        return () -> {};
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertFalse(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer1() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setHazardous(true);
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer2() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setDgClass("2.1");
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer3() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setUnNumber("un");
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer4() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setProperShippingName("psp");
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer5() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setPackingGroup("pg");
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer6() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setMinimumFlashPointUnit("CEL");
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer7() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setMinimumFlashPoint(BigDecimal.ONE);
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testCheckIfDGFieldsChangedInContainer8() {
        Containers containers = new Containers();
        ContainerRequest containerRequest = new ContainerRequest();
        containerRequest.setMarinePollutant(true);
        boolean response = commonUtils.checkIfDGFieldsChangedInContainer(containerRequest, containers);
        assertTrue(response);
    }

    @Test
    void testPopulateDictionaryForOceanDGCommercialApproval(){
        Map<String,Object> dictionary = new HashMap<>();
        CarrierDetails carrierDetails = CarrierDetails.builder().build();

        List<Containers> containersList = new ArrayList<>();
        containersList.add(Containers.builder().containerCount(10l).hazardous(true).build());

        List<Packing> packingList = new ArrayList<>();
        Packing packing = new Packing();
        packing.setPacks("10");
        packing.setHazardous(true);
        packing.setPacksType("AB");

        packingList.add(packing);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
            .carrierDetails(carrierDetails)
            .containersList(containersList)
            .packingList(packingList)
            .build();

        shipmentDetails.setId(1l);
        VesselsResponse vesselsResponse = new VesselsResponse();
        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().tasksId("qw").build();
        String remarks = "Remarks";

        List<AuditLog> auditLogList = new ArrayList<>();
        Map<String, AuditLogChanges> changes = new HashMap<>();
        AuditLogChanges timeAudit = AuditLogChanges.builder().fieldName(TIME).newValue("NOW").build();
        AuditLogChanges userNameAudit = AuditLogChanges.builder().fieldName(USERNAME).newValue(USERNAME).build();
        changes.put(TIME, timeAudit);
        changes.put(USERNAME, userNameAudit);
        auditLogList.add(AuditLog.builder().changes(changes).build());

        when(iAuditLogDao.findByOperationAndParentId(
            DBOperationType.DG_APPROVE.name(), shipmentDetails.getId())).thenReturn(auditLogList);
        commonUtils.populateDictionaryForOceanDGCommercialApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse);

        assertEquals("Remarks", dictionary.get(REQUESTER_REMARKS));
    }

    @Test
    void testGetDGEmailTemplate(){
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities("entities").build();

        when(iv1Service.getEmailTemplates(any())).thenReturn(v1DataResponse);
        List<EmailTemplatesRequest> emailTemplates = new ArrayList<>();
        EmailTemplatesRequest templateReq1 = EmailTemplatesRequest.builder()
            .type(OCEAN_DG_APPROVAL_REQUEST_EMAIL_TYPE)
            .build();
        emailTemplates.add(templateReq1);

        EmailTemplatesRequest templateReq2 = EmailTemplatesRequest.builder()
            .type(OCEAN_DG_APPROVAL_APPROVE_EMAIL_TYPE)
            .build();
        emailTemplates.add(templateReq2);

        EmailTemplatesRequest templateReq3 = EmailTemplatesRequest.builder()
            .type(OCEAN_DG_APPROVAL_REJECTION_EMAIL_TYPE)
            .build();
        emailTemplates.add(templateReq3);

        EmailTemplatesRequest templateReq4 = EmailTemplatesRequest.builder()
            .type(OCEAN_DG_COMMERCIAL_APPROVAL_REQUEST_EMAIL_TYPE)
            .build();
        emailTemplates.add(templateReq4);

        EmailTemplatesRequest templateReq5 = EmailTemplatesRequest.builder()
            .type(OCEAN_DG_COMMERCIAL_APPROVAL_APPROVE_EMAIL_TYPE)
            .build();
        emailTemplates.add(templateReq5);

        EmailTemplatesRequest templateReq6 = EmailTemplatesRequest.builder()
            .type(OCEAN_DG_COMMERCIAL_APPROVAL_REJECTION_EMAIL_TYPE)
            .build();
        emailTemplates.add(templateReq6);

        EmailTemplatesRequest templateReqOther = EmailTemplatesRequest.builder()
            .type("OTHER_EMAIL_TYPE")
            .build();
        emailTemplates.add(templateReqOther);

        when(jsonHelper.convertValueToList(v1DataResponse.entities, EmailTemplatesRequest.class)).thenReturn(emailTemplates);

        Map<OceanDGStatus, EmailTemplatesRequest> response = new HashMap<>();
        commonUtils.getDGEmailTemplate(response);
        assertNotNull(emailTemplates);
    }

    @Test
    void testGetRoleId(){
        OceanDGStatus oceanDGStatus = OCEAN_DG_REQUESTED;
        when(iv1Service.getRoleIdsByRoleName(any())).thenReturn(10);
        Integer roleId =  commonUtils.getRoleId(oceanDGStatus);
        assertEquals(10, roleId);
    }

    @Test
    void testGetUserEmailsByRoleId(){
        List<UsersRoleListResponse> userEmailResponse = new ArrayList<>();
        userEmailResponse.add(UsersRoleListResponse.builder().email("abc").build());
        when(iv1Service.getUserEmailsByRoleId(any())).thenReturn(userEmailResponse);

        List<String>  response = commonUtils.getUserEmailsByRoleId(1);
        assertNotNull(response);
    }

    @Test
    void testCreateTask_Success() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1l);
        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
        when( iv1Service.createTask(any())).thenReturn(taskCreateResponse);

        TaskCreateResponse response = commonUtils.createTask(shipmentDetails, 1);
        assertNotNull(response);
    }


    @Test
    void testGetVesselsData(){
        CarrierDetails carrierDetails = CarrierDetails.builder().vessel("vess").build();
        VesselsResponse vesselsResponse = new VesselsResponse();
        vesselsResponse.setName("Name");

        V1DataResponse vesselResponse = V1DataResponse.builder().build();
        when(iv1Service.fetchVesselData(any())).thenReturn(vesselResponse);

        List<VesselsResponse> vesselsResponseList = new ArrayList<>();
        vesselsResponseList.add(vesselsResponse);
        when(jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class)).thenReturn(vesselsResponseList);

        commonUtils.getVesselsData(carrierDetails, vesselsResponse);
        assertEquals("Name", vesselsResponse.getName());
    }

    @Test
    void testsendEmailResponseToDGRequester() throws RunnerException {
        Map<OceanDGStatus, EmailTemplatesRequest> emailTemplates = new HashMap<>();
        EmailTemplatesRequest emailTemplatesRequest = EmailTemplatesRequest.builder()
            .body("body")
            .subject("subject")
            .build();
        emailTemplates.put(OCEAN_DG_REQUESTED, emailTemplatesRequest);
        OceanDGRequest request = OceanDGRequest.builder().userEmail("ac").build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();

        commonUtils.sendEmailResponseToDGRequester(emailTemplatesRequest,request, shipmentDetails);
        assertEquals("ac", request.getUserEmail());
    }

    @Test
    void testCompareBigDecimals() {
        boolean response = commonUtils.compareBigDecimals(null, null);
        assertTrue(response);
    }

    @Test
    void testCompareBigDecimals1() {
        boolean response = commonUtils.compareBigDecimals(BigDecimal.TEN, null);
        assertFalse(response);
    }

    @Test
    void testCompareBigDecimals2() {
        boolean response = commonUtils.compareBigDecimals(null, BigDecimal.TEN);
        assertFalse(response);
    }

    @Test
    void testCompareBigDecimals3() {
        boolean response = commonUtils.compareBigDecimals(BigDecimal.TEN, new BigDecimal("10.00"));
        assertTrue(response);
    }

    @Test
    void testCompareBigDecimals4() {
        boolean response = commonUtils.compareBigDecimals(BigDecimal.ZERO, BigDecimal.TEN);
        assertFalse(response);
    }

    @Test
    void sendEmailForPullPushRequestStatusDetach() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_DETACH,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_DETACH, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusDetachNoTemplate() throws Exception {
        commonUtils.sendEmailForPullPushRequestStatus(
                ShipmentDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .build(),
                ConsolidationDetails.builder()
                        .carrierDetails(CarrierDetails.builder().build())
                        .allocations(Allocations.builder().build())
                        .build(),
                SHIPMENT_DETACH,
                "rejectRemarks",
                new HashMap<>(),
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                null, null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusDetachCases() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_DETACH,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_DETACH, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put("createdConsole", "createdConsole@gmail.com");
                    put("assigned", "assigned@gmail.com");
                    put("createdShipment", "createdShipment@gmail.com");
                    put("username", "username@gmail.com");
                }},
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void sendEmailForPullPushRequestStatusDetachCases2() throws Exception {
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .allocations(Allocations.builder().build())
                .build();
        consolidationDetails1.setCreatedBy("createdConsole");
        consolidationDetails1.setTenantId(56);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build())
                .assignedTo("assigned")
                .build();
        shipmentDetails.setCreatedBy("createdShipment");
        shipmentDetails.setTenantId(56);
        UserContext.getUser().setEmail(null);
        V1TenantSettingsResponse v1TenantSettingsResponse = V1TenantSettingsResponse.builder().build();
        commonUtils.sendEmailForPullPushRequestStatus(
                shipmentDetails,
                consolidationDetails1,
                SHIPMENT_DETACH,
                "rejectRemarks",
                new HashMap<>() {{
                    put(SHIPMENT_DETACH, EmailTemplatesRequest.builder().body("").subject("").build());
                }},
                new HashSet<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>() {{
                    put(56, v1TenantSettingsResponse);
                }},
                "username", null);
        verify(notificationService, times(0)).sendEmail(any(), any(), any(), any());
    }

    @Test
    void testRemoveDuplicateTrackingEvents_NoDuplicates() {
        List<Events> eventsList = new ArrayList<>();
        Events e1 = Events.builder().eventCode("E1").containerNumber("C1").shipmentNumber("S1").source("SRC1").build();
        Events e2 = Events.builder().eventCode("E2").containerNumber("C2").shipmentNumber("S2").source("SRC2").build();
        eventsList.add(e1);
        eventsList.add(e2);

        commonUtils.removeDuplicateTrackingEvents(eventsList);

        // No events should be removed since all are unique
        assertEquals(2, eventsList.size());
    }

    @Test
    void testRemoveDuplicateTrackingEvents_WithDuplicates() {
        List<Events> eventsList = new ArrayList<>();
        Events e1 = Events.builder().eventCode("E1").containerNumber("C1").shipmentNumber("S1").source("SRC1").build();
        Events e2 = Events.builder().eventCode("E2").containerNumber("C2").shipmentNumber("S2").source("SRC2").build();
        eventsList.add(e1);
        eventsList.add(e2);
        eventsList.add(e1);
        eventsList.add(e2);

        commonUtils.removeDuplicateTrackingEvents(eventsList);

        // One duplicate should be removed, expect only two unique events
        assertEquals(2, eventsList.size());
    }

    @Test
    void testRemoveDuplicateTrackingEvents_NullList() {
        List<Events> events = null;

        commonUtils.removeDuplicateTrackingEvents(events);

        // Should not throw an exception, list is null, nothing happens
        // In this case, we just check that no exception is thrown.
        assertNull(events);
    }

    @Test
    void testGetTrackingEventsUniqueKey_AllValidInputs() {
        String eventCode = "E123";
        String containerNumber = "C456";
        String shipmentNumber = "S789";
        String source = "SRC";
        String place = "place";

        String result = commonUtils.getTrackingEventsUniqueKey(eventCode, containerNumber, shipmentNumber, source, place);
        assertEquals("E123-C456-S789-SRC-place", result);
    }

    @Test
    void testGetTrackingEventsUniqueKey_NullContainerNumber() {
        String eventCode = "E123";
        String containerNumber = null;  // Testing with null value
        String shipmentNumber = "S789";
        String source = "SRC";
        String place = "place";

        String result = commonUtils.getTrackingEventsUniqueKey(eventCode, containerNumber, shipmentNumber, source, place);
        assertEquals("E123--S789-SRC-place", result);  // Expect containerNumber to be empty
    }

    @Test
    void testPopulateTemplate_withValidData_shouldPopulateDictionary() {

        Map<String, Object> dictionary = new HashMap<>();
        when(v1TenantSettingsResponse.getDPWDateFormat()).thenReturn("MM/dd/yyyy");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);
        // Set up shipment details
        when(shipmentDetails.getCreatedBy()).thenReturn("creator");
        when(shipmentDetails.getAssignedTo()).thenReturn("assignee");
        when(shipmentDetails.getShipmentId()).thenReturn("SHIP123");
        when(shipmentDetails.getId()).thenReturn(1L);


        // Set up consolidation details
        when(consolidationDetails.getConsolidationNumber()).thenReturn("CONS123");
        when(consolidationDetails.getMawb()).thenReturn("MAWB123");
        when(consolidationDetails.getCarrierDetails()).thenReturn(carrierDetailsMock);
        when(carrierDetailsMock.getFlightNumber()).thenReturn("FL123");

        when(consolidationDetails.getCarrierDetails().getOriginPort()).thenReturn("origin_port");
        when(consolidationDetails.getCarrierDetails().getShippingLine()).thenReturn("abc");
        when(consolidationDetails.getCarrierDetails().getDestinationPort()).thenReturn("destination_port");

        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        unLocMap.put("origin_port", new UnlocationsResponse());
        carrierMasterDataMap.put("abc", new CarrierMasterData());
        unLocMap.put("destination_port", new UnlocationsResponse());

        // Call the method
        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);

        // Verify dictionary population
        assertEquals("creator", dictionary.get(SHIPMENT_CREATE_USER));
        assertEquals("assignee", dictionary.get(SHIPMENT_ASSIGNED_USER));
        assertEquals("SHIP123", dictionary.get(INTERBRANCH_SHIPMENT_NUMBER_WITHOUT_LINK));
        assertEquals("CONS123", dictionary.get(SOURCE_CONSOLIDATION_NUMBER));
        assertEquals("MAWB123", dictionary.get(Constants.MAWB_NUMBER));
        assertEquals("FL123", dictionary.get(FLIGHT_NUMBER1));

        // Verify UserContext and tenant details
        assertEquals("TEST_CODE", dictionary.get(CONSOL_BRANCH_CODE));
        assertEquals("Test Tenant", dictionary.get(CONSOL_BRANCH_NAME));
        assertEquals("TestUser", dictionary.get(ACTIONED_USER_NAME));
    }

    @Test
    void testPopulateShipmentImportPushAttachmentTemplate_withValidData() {

        Map<String, Object> dictionary = new HashMap<>();
        when(v1TenantSettingsResponse.getDPWDateFormat()).thenReturn("MM/dd/yyyy");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);

        // Mock shipmentDetails
        when(shipmentDetails.getShipmentId()).thenReturn("SHIP123");
        when(shipmentDetails.getHouseBill()).thenReturn("HB123");
        when(shipmentDetails.getCarrierDetails()).thenReturn(carrierDetailsMock);

        // Mock consolidationDetails
        when(consolidationDetails.getCreatedBy()).thenReturn("createdByUser");
        when(consolidationDetails.getConsolidationNumber()).thenReturn("CONSOL123");

        // Mock carrier details
        when(carrierDetailsMock.getEtd()).thenReturn(LocalDate.parse( "2024-09-20").atTime(LocalTime.MIDNIGHT));
        when(carrierDetailsMock.getEta()).thenReturn(LocalDate.parse( "2024-09-25").atTime(LocalTime.MIDNIGHT));
        when(carrierDetailsMock.getShippingLine()).thenReturn("LINE123");
        when(carrierDetailsMock.getFlightNumber()).thenReturn("FLIGHT001");
        when(carrierDetailsMock.getOriginPort()).thenReturn("ORIG001");
        when(carrierDetailsMock.getDestinationPort()).thenReturn("DEST001");
        when(shipmentDetails.getCarrierDetails().getOriginPort()).thenReturn("origin_port");
        when(shipmentDetails.getCarrierDetails().getShippingLine()).thenReturn("abc");
        when(shipmentDetails.getCarrierDetails().getDestinationPort()).thenReturn("destination_port");

        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        unLocMap.put("origin_port", new UnlocationsResponse());
        carrierMasterDataMap.put("abc", new CarrierMasterData());
        unLocMap.put("destination_port", new UnlocationsResponse());

        // Test method
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);

        // Assert dictionary contents
        assertEquals("createdByUser", dictionary.get(CONSOLIDATION_CREATE_USER));
        assertEquals("SHIP123", dictionary.get(Constants.SHIPMENT_NUMBER));
        assertEquals("Test Tenant", dictionary.get(CONSOL_BRANCH_NAME));
        assertEquals("HB123", dictionary.get(Constants.HAWB_NUMBER));
        assertEquals("09/20/2024", dictionary.get(ETD_CAPS));
        assertEquals("09/25/2024", dictionary.get(ETA_CAPS));
        assertEquals("FLIGHT001", dictionary.get(FLIGHT_NUMBER1));
        assertEquals("TEST_CODE", dictionary.get(CONSOL_BRANCH_CODE));
        assertEquals("TestUser", dictionary.get(ACTIONED_USER_NAME));
    }

    @Test
    void testPopulateShipmentImportPushAttachmentTemplate_withMissingCarrierDetails() {

        Map<String, Object> dictionary = new HashMap<>();
        when(v1TenantSettingsResponse.getDPWDateFormat()).thenReturn("MM/dd/yyyy");
        when(commonUtils.getCurrentTenantSettings()).thenReturn(v1TenantSettingsResponse);

        // Mock shipmentDetails with missing carrier details
        when(shipmentDetails.getCarrierDetails()).thenReturn(carrierDetailsMock);
        when(carrierDetailsMock.getShippingLine()).thenReturn(null);

        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();

        // Test method
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails, consolidationDetails, carrierMasterDataMap, unLocMap);

        // Assert dictionary does not contain carrier details
        assertNull(dictionary.get("CARRIER_CODE"));
        assertNull(dictionary.get("CARRIER_NAME"));
    }

    @Test
    void testSendEmailNotification_WithEmptyToList() {
        Map<String, Object> dictionary = new HashMap<>();
        // Arrange
        List<String> to = new ArrayList<>();  // Empty "to" list
        List<String> cc = Arrays.asList("cc@example.com");

        // Act
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, to, cc);

        // Assert
        // Ensure notificationService.sendEmail is never called
        verify(notificationService, never()).sendEmail(anyString(), anyString(), anyList(), anyList());
    }


    @Test
    void testSendEmailNotification_WhenToListIsEmpty() {
        Map<String, Object> dictionary = new HashMap<>();
        List<String> ccEmails = new ArrayList<>();
        // Arrange: empty 'to' list
        List<String> emptyTo = List.of();

        // Act
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, emptyTo, ccEmails);

        // Assert: verify that the notification service sendEmail was never called
        verify(notificationService, never()).sendEmail(anyString(), anyString(), anyList(), anyList());
    }

    @Test
    void testSendEmailNotification_ValidatesDictionaryModification() {
        Map<String, Object> dictionary = new HashMap<>();
        emailTemplateModel = new EmailTemplatesRequest();
        emailTemplateModel.setBody("Hello, {name}");
        emailTemplateModel.setSubject("Shipment Update");

        // Arrange
        List<String> to = List.of("recipient@example.com");
        List<String> cc = List.of("cc@example.com");
        dictionary.put("name", "John Doe");

        // Act
        commonUtils.sendEmailNotification(dictionary, emailTemplateModel, to, cc);

        // Assert
        assertEquals("Hello, John Doe", commonUtils.replaceTagsFromData(dictionary, emailTemplateModel.getBody()));
    }

    @Test
    void testGetEmailTemplates_Success() {
        // Arrange: Mock the iv1Service response
        V1DataResponse mockV1DataResponse = new V1DataResponse();
        mockV1DataResponse.entities = new ArrayList<>();  // Add any mock entities you need

        when(iv1Service.getEmailTemplates(any(CommonV1ListRequest.class))).thenReturn(mockV1DataResponse);

        // Mock the jsonHelper conversion
        List<EmailTemplatesRequest> mockTemplates = new ArrayList<>();
        when(jsonHelper.convertValueToList(mockV1DataResponse.entities, EmailTemplatesRequest.class)).thenReturn(mockTemplates);

        // Act
        List<EmailTemplatesRequest> result = commonUtils.getEmailTemplates("TestTemplateType");

        // Assert: Verify the result and interactions
        assertNotNull(result);
        assertEquals(mockTemplates, result);  // Result should match the mock list returned by the helper

        // Verify that the service and helper were called with the correct arguments
        verify(iv1Service, times(1)).getEmailTemplates(any(CommonV1ListRequest.class));
        verify(jsonHelper, times(1)).convertValueToList(mockV1DataResponse.entities, EmailTemplatesRequest.class);
    }

    @Test
    void testGetEmailTemplates_EmptyResponse() {
        // Arrange: Mock an empty response
        V1DataResponse emptyResponse = new V1DataResponse();
        emptyResponse.entities = new ArrayList<>();  // No entities in the response

        when(iv1Service.getEmailTemplates(any(CommonV1ListRequest.class))).thenReturn(emptyResponse);

        when(jsonHelper.convertValueToList(emptyResponse.entities, EmailTemplatesRequest.class)).thenReturn(new ArrayList<>());

        // Act
        List<EmailTemplatesRequest> result = commonUtils.getEmailTemplates("EmptyTemplateType");

        // Assert: Verify the result is an empty list
        assertNotNull(result);
        assertTrue(result.isEmpty());

        // Verify the service and helper were called correctly
        verify(iv1Service, times(1)).getEmailTemplates(any(CommonV1ListRequest.class));
        verify(jsonHelper, times(1)).convertValueToList(emptyResponse.entities, EmailTemplatesRequest.class);
    }

    @Test
    void testGetEmailTemplates_ExceptionThrown() {
        // Arrange: Mock the service to throw an exception
        when(iv1Service.getEmailTemplates(any(CommonV1ListRequest.class)))
                .thenThrow(new RuntimeException("Error fetching templates"));

        // Act & Assert: Verify that an exception is thrown and handled properly
        assertThrows(RuntimeException.class, () -> {
            commonUtils.getEmailTemplates("TestTemplateType");
        });

        // Verify that the helper was never called since the service failed
        verify(jsonHelper, never()).convertValueToList(anyList(), eq(EmailTemplatesRequest.class));
    }

    @Test
    void testIsTransportModeValid() {
        var tenantSettings = V1TenantSettingsResponse.builder().build();
        var response = commonUtils.isTransportModeValid(TRANSPORT_MODE_SEA, SHIPMENT_DETAILS, tenantSettings);
        assertFalse(response);
    }

    @ParameterizedTest
    @ValueSource(strings = {SHIPMENT_DETAILS, CUSTOMER_BOOKING, CONSOLIDATION})
    void testIsTransportModeValidSea(String entity) {
        var tenantSettings = V1TenantSettingsResponse.builder().shipmentTransportModeAir(true).shipmentTransportModeRail(true).shipmentTransportModeRoad(true).bookingTransportModeAir(true).bookingTransportModeRail(true).bookingTransportModeRoad(true).build();
        var response = commonUtils.isTransportModeValid(TRANSPORT_MODE_SEA, entity, tenantSettings);
        assertFalse(response);
    }

    @ParameterizedTest
    @ValueSource(strings = {SHIPMENT_DETAILS, CUSTOMER_BOOKING})
    void testIsTransportModeValidAir(String entity) {
        var tenantSettings = V1TenantSettingsResponse.builder().shipmentTransportModeSea(true).shipmentTransportModeRail(true).shipmentTransportModeRoad(true).bookingTransportModeSea(true).bookingTransportModeRail(true).bookingTransportModeRoad(true).build();
        var response = commonUtils.isTransportModeValid(TRANSPORT_MODE_AIR, entity, tenantSettings);
        assertFalse(response);
    }

    @ParameterizedTest
    @ValueSource(strings = {SHIPMENT_DETAILS, CUSTOMER_BOOKING})
    void testIsTransportModeValidRail(String entity) {
        var tenantSettings = V1TenantSettingsResponse.builder().shipmentTransportModeSea(true).shipmentTransportModeAir(true).shipmentTransportModeRoad(true).bookingTransportModeSea(true).bookingTransportModeAir(true).bookingTransportModeRoad(true).build();
        var response = commonUtils.isTransportModeValid(TRANSPORT_MODE_RAI, entity, tenantSettings);
        assertFalse(response);
    }

    @ParameterizedTest
    @ValueSource(strings = {SHIPMENT_DETAILS, CUSTOMER_BOOKING})
    void testIsTransportModeValidRoad(String entity) {
        var tenantSettings = V1TenantSettingsResponse.builder().shipmentTransportModeSea(true).shipmentTransportModeSea(true).shipmentTransportModeRail(true).bookingTransportModeSea(true).bookingTransportModeSea(true).bookingTransportModeRail(true).build();
        var response = commonUtils.isTransportModeValid(TRANSPORT_MODE_ROA, entity, tenantSettings);
        assertFalse(response);
    }

    @ParameterizedTest
    @ValueSource(strings = {SHIPMENT_DETAILS, CUSTOMER_BOOKING})
    void testIsTransportModeValidAccepted(String entity) {
        var tenantSettings = V1TenantSettingsResponse.builder().shipmentTransportModeSea(true).bookingTransportModeSea(true).build();
        var response = commonUtils.isTransportModeValid(TRANSPORT_MODE_SEA, entity, tenantSettings);
        assertTrue(response);
    }

    @Test
    void testUpdateEventWithMasterData() {
        String mockEventCode = "EV1";
        String mockEventDescription = "mock description";
        String mockEventDescription2 = "mock description 2";
        Events mockEvent1 = Events.builder().eventCode("EV1").build();
        Events mockEvent2 = Events.builder().description(mockEventDescription2).build();
        List<Events> mockEventList = List.of(mockEvent1, mockEvent2);

        V1DataResponse mockV1DataResponse = new V1DataResponse();
        when(iv1Service.fetchMasterData(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferMasterLists.class))).thenReturn(List.of(
                EntityTransferMasterLists.builder().ItemValue(mockEventCode).ItemDescription(mockEventDescription).build()
        ));

        commonUtils.updateEventWithMasterData(mockEventList);

        assertEquals(mockEventDescription, mockEvent1.getDescription());
        assertEquals(mockEventDescription2, mockEvent2.getDescription());
    }

    @Test
    void testUpdateEventWithMasterDataDescriptionKeepsTheOlderInCaseOfExceptionFromV1() {
        String mockEventCode = "EV1";
        String mockEventDescription = "older description";
        Events mockEvent = Events.builder().eventCode(mockEventCode).description(mockEventDescription).build();
        List<Events> mockEventList = List.of(mockEvent);

        when(iv1Service.fetchMasterData(any())).thenThrow(new RuntimeException("mock error !"));

        commonUtils.updateEventWithMasterData(mockEventList);

        assertEquals(mockEventDescription, mockEvent.getDescription());
    }

    @Test
    void testUpdateEventWithMasterDataDescriptionDoesNotFailsIfEventCodeIsNull() {
        String mockEventDescription = "non updatable description";
        Events mockEvent = Events.builder().description(mockEventDescription).build();
        List<Events> mockEventList = List.of(mockEvent);

        commonUtils.updateEventWithMasterData(mockEventList);

        assertEquals(mockEventDescription, mockEvent.getDescription());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "I"})
    void testGetCountryFromUnLocCode(String req) {
        String response = commonUtils.getCountryFromUnLocCode(req);
        assertNull(response);
    }

    @Test
    void testGetCountryFromUnLocCode1() {
        String response = commonUtils.getCountryFromUnLocCode("IN");
        assertEquals("IND", response);
    }

    @ParameterizedTest
    @MethodSource("providePartiesObjects")
    void testCheckIfPartyExists(Parties req) {
        boolean response = commonUtils.checkIfPartyExists(req);
        assertFalse(response);
    }

    @ParameterizedTest
    @MethodSource("providePartiesResponseObjects")
    void testCheckIfPartyExists1(PartiesResponse req) {
        boolean response = commonUtils.checkIfPartyExists(req);
        assertFalse(response);
    }

    @Test
    void testCheckIfPartyExists2() {
        PartiesResponse req = new PartiesResponse();
        req.setOrgCode("orgCode");
        boolean response = commonUtils.checkIfPartyExists(req);
        assertTrue(response);
    }

    @Test
    void testCheckIfPartyExists3() {
        Parties req = new Parties();
        req.setOrgCode("orgCode");
        boolean response = commonUtils.checkIfPartyExists(req);
        assertTrue(response);
    }

    private static Stream<Parties> providePartiesObjects() {
        return Stream.of(
                new Parties(),
                null
        );
    }

    private static Stream<PartiesResponse> providePartiesResponseObjects() {
        return Stream.of(
                new PartiesResponse(),
                null
        );
    }

    @Test
    void testMandatoryHsCodeForUAE_withShipmentIdAndMissingHsCode_shouldThrowValidationException() {

        Awb awb = new Awb();
        CarrierDetails carrierDetailsWithAE = mock(CarrierDetails.class);
        when(carrierDetailsWithAE.getDestinationPortLocCode()).thenReturn("AE123");
        when(shipmentDetails.getCarrierDetails()).thenReturn(carrierDetailsWithAE);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        awb.setShipmentId(1L);
        AwbGoodsDescriptionInfo goodsWithoutHsCode = new AwbGoodsDescriptionInfo();
        awb.setAwbGoodsDescriptionInfo(Arrays.asList(goodsWithoutHsCode));

        assertThrows(ValidationException.class, () -> commonUtils.checkForMandatoryHsCodeForUAE(awb));
    }

    @Test
    void testMandatoryHsCodeForUAE_withShipmentIdAndMissingHsCode_shouldThrowValidationException1() {

        Awb awb = new Awb();
        CarrierDetails carrierDetailsWithoutAE = mock(CarrierDetails.class);
        when(carrierDetailsWithoutAE.getDestinationPortLocCode()).thenReturn("US456");

        when(shipmentDetails.getCarrierDetails()).thenReturn(carrierDetailsWithoutAE);
        when(shipmentDao.findById(any())).thenReturn(Optional.of(shipmentDetails));

        awb.setShipmentId(1L);
        AwbGoodsDescriptionInfo goodsWithoutHsCode = new AwbGoodsDescriptionInfo();
        awb.setAwbGoodsDescriptionInfo(Arrays.asList(goodsWithoutHsCode));

        commonUtils.checkForMandatoryHsCodeForUAE(awb);
    }

    @Test
    void testMandatoryHsCodeForUAE_withConsolidationIdAndMissingHsCode_shouldThrowValidationException() {

        Awb awb = new Awb();
        CarrierDetails carrierDetailsWithAE = mock(CarrierDetails.class);
        when(carrierDetailsWithAE.getDestinationPortLocCode()).thenReturn("AE123");
        when(consolidationDetails.getCarrierDetails()).thenReturn(carrierDetailsWithAE);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));

        awb.setConsolidationId(1L);
        AwbGoodsDescriptionInfo goodsWithoutHsCode = new AwbGoodsDescriptionInfo();
        awb.setAwbGoodsDescriptionInfo(Arrays.asList(goodsWithoutHsCode));

        assertThrows(ValidationException.class, () -> commonUtils.checkForMandatoryHsCodeForUAE(awb));
    }

    @Test
    void testMandatoryHsCodeForUAE_withConsolidationIdAndMissingHsCode_shouldThrowValidationException1() {

        Awb awb = new Awb();
        CarrierDetails carrierDetailsWithoutAE = mock(CarrierDetails.class);
        when(carrierDetailsWithoutAE.getDestinationPortLocCode()).thenReturn("US456");
        when(consolidationDetails.getCarrierDetails()).thenReturn(carrierDetailsWithoutAE);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolidationDetails));

        awb.setConsolidationId(1L);
        AwbGoodsDescriptionInfo goodsWithoutHsCode = new AwbGoodsDescriptionInfo();
        awb.setAwbGoodsDescriptionInfo(Arrays.asList(goodsWithoutHsCode));

        commonUtils.checkForMandatoryHsCodeForUAE(awb);
    }

    @Test
    void testMandatoryHsCodeForUAE_withBothShipmentAndConsolidationIdNull_shouldNotThrowException() {

        Awb awb = new Awb();
        AwbGoodsDescriptionInfo goodsWithoutHsCode = new AwbGoodsDescriptionInfo();
        awb.setAwbGoodsDescriptionInfo(Arrays.asList(goodsWithoutHsCode));
        commonUtils.checkForMandatoryHsCodeForUAE(awb);
    }

    @Test
    void testGetAutoPopulateDepartmentReturnSingleUniqueDepartmentValue() {
        String transportMode = "AIR";
        String direction = "EXP";
        String module = "SHP";

        when(mdmServiceAdapter.getDepartmentList(anyString(), anyString(), anyString())).thenReturn(List.of(
                Map.ofEntries(Map.entry(MdmConstants.DEPARTMENT, "AE")),
                Map.ofEntries(Map.entry(MdmConstants.DEPARTMENT, "AE")),
                Map.ofEntries(Map.entry(MdmConstants.DEPARTMENT, "AE"))
        ));

        String res = commonUtils.getAutoPopulateDepartment(transportMode, direction, module);
        assertEquals("AE", res);
    }

    @Test
    void testGetAutoPopulateDepartmentReturnsNullIfMoreThanSingleUniqueDepartment() {
        String transportMode = "AIR";
        String direction = "EXP";
        String module = "SHP";

        when(mdmServiceAdapter.getDepartmentList(anyString(), anyString(), anyString())).thenReturn(List.of(
                Map.ofEntries(Map.entry(MdmConstants.DEPARTMENT, "AE")),
                Map.ofEntries(Map.entry(MdmConstants.DEPARTMENT, "AE")),
                Map.ofEntries(Map.entry(MdmConstants.DEPARTMENT, "ACT"))
        ));

        String res = commonUtils.getAutoPopulateDepartment(transportMode, direction, module);
        assertNull(res);
    }

    @Test
    void testGetAutoPopulateDepartmentReturnsNullIfNoResponseFromMDM() {
        String transportMode = "AIR";
        String direction = "EXP";
        String module = "SHP";

        when(mdmServiceAdapter.getDepartmentList(anyString(), anyString(), anyString())).thenReturn(Collections.emptyList());

        String res = commonUtils.getAutoPopulateDepartment(transportMode, direction, module);
        assertNull(res);
    }

    @Test
    void testGetShipmentDetailsResponse() {
        List<String> includeColumns = List.of("carrierDetails", "eventsList");
        Object response = commonUtils.getShipmentDetailsResponse(shipmentDetails, includeColumns);
        assertNotNull(response);
    }

    @Test
    void testGetShipmentDetailsResponseWithEmptyString() {
        List<String> includeColumns = List.of(StringUtility.getEmptyString());
        Object response = commonUtils.getShipmentDetailsResponse(shipmentDetails, includeColumns);
        assertNotNull(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd() {
        assertFalse(commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().direction(IMP).build(), false));
    }
}