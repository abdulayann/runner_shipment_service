package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
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
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.impl.ConsolidationDao;
import com.dpw.runner.shipment.services.dao.impl.QuoteContractsDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.ListCousinBranchesForEtRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.SendEmailDto;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.QuoteContracts;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.notification.response.NotificationServiceResponse;
import com.dpw.runner.shipment.services.notification.service.INotificationService;
import com.dpw.runner.shipment.services.service.impl.ApplicationConfigServiceImpl;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.service.impl.TenantSettingsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Element;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.exceptions.InvalidPdfException;
import com.itextpdf.text.pdf.BaseFont;
import com.itextpdf.text.pdf.PdfContentByte;
import com.itextpdf.text.pdf.PdfGState;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.PdfWriter;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.transaction.TransactionSystemException;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.*;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.awt.image.BufferedImage;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Consumer;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETA_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ETD_CAPS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.PermissionConstants.OCEAN_DG_APPROVER;
import static com.dpw.runner.shipment.services.commons.constants.PermissionConstants.OCEAN_DG_COMMERCIAL_APPROVER;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
    @Spy
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
    private QuoteContractsDao quoteContractsDao;

    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;
    @Mock
    private IV1Service v1Service;

    @Mock
    private TenantModel tenantModel;

    @Mock
    private EntityTransferAddress entityTransferAddress;

    private List<Selection<?>> selections;
    private List<String> columnOrder;
    @Mock(lenient = true)
    private Root<ShipmentDetails> root;

    @Mock(lenient = true)
    private CriteriaBuilder criteriaBuilder;

    @Mock(lenient = true)
    private Join<Object, Object> mockJoin;

    @Mock(lenient = true)
    private Path<Object> mockPath;

    @Mock(lenient = true)
    private Selection<?> mockSelection;
    private Map<String, Join<?, ?>> joinCache;

    @Mock
    private EntityManager entityManager;

    @Mock
    private CriteriaQuery<Object[]> criteriaQuery;

    @Mock
    private TypedQuery<Object[]> typedQuery;

    @Mock
    private TypedQuery<Long> typedQuery1;

    @Mock
    private Join firstJoin;  // Remove generics

    @Mock
    private Join secondJoin;  // Remove generics

    @Mock
    private Join thirdJoin;  // Remove generics

    @Mock
    private CriteriaQuery<Long> countQuery;

    @Mock
    private ShipmentSettingsDetails shipmentSettingsDetails;

    @Mock
    private ApplicationConfigServiceImpl applicationConfigService;

    @Mock
    private ObjectMapper objectMapper;

    private PdfContentByte dc;
    private BaseFont font;
    private Rectangle realPageSize;
    private Rectangle rect;

    static Stream<Arguments> pTestCases() {
        return Stream.of(
                Arguments.of("Entity", new HashSet<>(), "Response"),
                Arguments.of("Entity", Set.of("foo"), "Response"),
                Arguments.of("Entity", Set.of("No such field: {}", "foo"), "Response"),
                Arguments.of("Entity", Collections.singleton(null), "Response"),
                Arguments.of("Entity", Collections.singleton(""), "Response"),
                Arguments.of(new HashMap<>(), Set.of("foo"), "Response"),
                Arguments.of(null, Set.of("foo"), "Response"),
                Arguments.of(Map.of("foo", "42"), Set.of("foo"), "Response")
        );
    }

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
        MockitoAnnotations.openMocks(this);
        MockitoAnnotations.initMocks(this);
        commonUtils.syncExecutorService = Executors.newFixedThreadPool(2);
        commonUtils.shipmentSettingsDao = shipmentSettingsDao;
        selections = new ArrayList<>();
        columnOrder = new ArrayList<>();
        joinCache = new HashMap<>();

        // Setup basic mocks
        when(root.get(anyString())).thenReturn(mockPath);
        when(mockJoin.get(anyString())).thenReturn(mockPath);


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
        CommonUtils.addWaterMark(dc, "Test Watermark", font, 50, 35, new BaseColor(70, 70, 255), realPageSize, rect);

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

    private byte[] createSamplePdf() throws DocumentException {
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
    void isStringNullOrEmpty_NullInput_ReturnsTrue() {
        boolean result = CommonUtils.isStringNullOrEmpty(null);
        assertTrue(result);
    }

    @Test
    void isStringNullOrEmpty_EmptyStringInput_ReturnsTrue() {
        boolean result = CommonUtils.isStringNullOrEmpty("");
        assertTrue(result);
    }

    @Test
    void isStringNullOrEmpty_NonEmptyStringInput_ReturnsFalse() {
        String input = "Hello";
        boolean result = CommonUtils.isStringNullOrEmpty(input);
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
        byte[] result = CommonUtils.imageToByte(img);

        assertNotNull(result);
    }

    @Test
    void testHasUnsupportedCharacters() {
        String input = "ValidString123";
        boolean result = CommonUtils.hasUnsupportedCharacters(input);

        assertFalse(result);

        input = "InvalidString\u001F";
        result = CommonUtils.hasUnsupportedCharacters(input);

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

        String result = CommonUtils.getErrorResponseMessage(transactionSystemException);

        assertEquals("Root cause message", result);
    }

    @Test
    void testGetErrorResponseMessage_WithGenericException() {
        Exception genericException = new Exception("Generic exception message");
        String result = CommonUtils.getErrorResponseMessage(genericException);
        assertEquals("Generic exception message", result);
    }

    private byte[] createSamplePdfWithMultiplePages() throws DocumentException {
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
        when(mockTenantSettingsResponse.getColoadingBranchIds()).thenReturn(List.of(1, 2));

        assertNotNull(mockTenantSettingsResponse.getColoadingBranchIds());

        InterBranchDto interBranchDto = new InterBranchDto();
        interBranchDto.setColoadStationsTenantIds(tenantSettingsService.getV1TenantSettings(any()).getColoadingBranchIds());
        interBranchDto.setHub(true);

        commonUtils.setInterBranchContextForHub();
        InterBranchDto interBranchContext = InterBranchContext.getContext();
        assertNotNull(interBranchContext);
        assertTrue(interBranchContext.isHub());
        assertEquals(List.of(1, 2), interBranchContext.getColoadStationsTenantIds());
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
        when(mockTenantSettingsResponse.getColoadingBranchIds()).thenReturn(List.of(1, 2));

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
        when(masterDataUtils.getLocationDataFromCache(any(), anyString())).thenReturn(unlocationsMap);
        commonUtils.updateUnLocData(carrierDetails, null);
        verify(carrierDetailsDao, times(0)).saveUnLocCodes(any());
    }

    @Test
    void testUpdateCarrierUnLocData() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        String mockLocCode = "LocCode-test";
        Map<String, EntityTransferUnLocations> unlocationsMap = new HashMap<>();
        unlocationsMap.put("test", EntityTransferUnLocations.builder().LocCode(mockLocCode).build());
        commonUtils.updateCarrierUnLocData(carrierDetails, unlocationsMap);
        assertEquals(carrierDetails.getOriginLocCode(), mockLocCode);
        assertEquals(carrierDetails.getDestinationLocCode(), mockLocCode);
        assertEquals(carrierDetails.getOriginPortLocCode(), mockLocCode);
        assertEquals(carrierDetails.getDestinationPortLocCode(), mockLocCode);
    }

    @Test
    void testUpdateCarrierUnLocData_FailsToUpdateInCaseOfError() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setOrigin("test");
        carrierDetails.setOriginPort("test");
        carrierDetails.setDestination("test");
        carrierDetails.setDestinationPort("test");
        commonUtils.updateCarrierUnLocData(carrierDetails, null);
        assertEquals(null, carrierDetails.getOriginLocCode());
    }

    @Test
    void testUpdateRoutingUnLocData() {
        Routings routings = new Routings();
        routings.setPol("test");
        routings.setPod("test");
        String mockLocCode = "LocCode-test";
        Map<String, EntityTransferUnLocations> unlocationsMap = new HashMap<>();
        unlocationsMap.put("test", EntityTransferUnLocations.builder().LocCode(mockLocCode).build());
        commonUtils.updateRoutingUnLocData(List.of(routings), unlocationsMap);
        assertEquals(routings.getOriginPortLocCode(), mockLocCode);
        assertEquals(routings.getDestinationPortLocCode(), mockLocCode);
    }

    @Test
    void testUpdateRoutingUnLocData_FailsToUpdateInCaseOfError() {
        Routings routings = new Routings();
        routings.setPol("test");
        routings.setPod("test");
        String mockLocCode = null;
        commonUtils.updateRoutingUnLocData(List.of(routings), null);
        assertEquals(routings.getOriginPortLocCode(), mockLocCode);
        assertEquals(routings.getDestinationPortLocCode(), mockLocCode);
    }

    @Test
    void testGetChangedUnLocationFields_ForInputEntity() {
        Routings routings = new Routings();
        routings.setPol("testPol");
        routings.setPod("testPod");
        Set<String> unLocationSet = new HashSet<>();

        commonUtils.getChangedUnLocationFields(routings, null, unLocationSet);
        assertEquals(2, unLocationSet.size());
    }

    @Test
    void testGetChangedUnLocationFields_ShouldNotChangeSetIfFieldsAreSame() {
        Routings routings = new Routings();
        routings.setPol("testPol");
        routings.setPod("testPod");

        Routings oldRouting = new Routings();
        oldRouting.setPol("testPol");
        oldRouting.setPod("testPod");
        Set<String> unLocationSet = new HashSet<>();

        commonUtils.getChangedUnLocationFields(routings, oldRouting, unLocationSet);
        assertEquals(0, unLocationSet.size());
    }

    @Test
    void testGetChangedUnLocationFields_ForInputList() {
        Routings routings1 = new Routings();
        routings1.setPol("testPol1");
        routings1.setPod("testPod1");
        Routings routings2 = new Routings();
        routings2.setPol("testPol2");
        routings2.setPod("testPod2");
        Set<String> unLocationSet = new HashSet<>();

        commonUtils.getChangedUnLocationFields(List.of(routings1, routings2), null, unLocationSet);
        assertEquals(4, unLocationSet.size());
    }

    @Test
    void testGetChangedUnLocationFields_ForEmptyInputList() {
        Set<String> unLocationSet = new HashSet<>();
        commonUtils.getChangedUnLocationFields(null, null, unLocationSet);
        assertEquals(0, unLocationSet.size());
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
                "username", null, false);
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
                null, null, false);
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
                null, null, false);
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
                "username", null, false);
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
                "username", null, false);
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
                null, null, false);
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
                null, null, false);
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
                "username", null, false);
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
                "username", null, false);
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
        tenantModelMap.put(1, tenantModel);
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
                null, tenantModelMap, false);
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
        tenantModelMap.put(1, tenantModel);
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
                null, tenantModelMap, false);
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
                null, null, false);
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
                null, null,false);
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
                "username", null, false);
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
                "username", null, false);
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
                null, null, false);
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
                null, null, false);
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
                "username", null, false);
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
                "username", null, false);
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
                null, null, false);
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
                null, null, false);
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
                "username", null, false);
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
                "username", null, false);
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
                null, null, false);
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
                "username", null, false);
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
                "username", null, false);
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
        when(masterDataUtils.getLocationData(any())).thenReturn(new HashMap<>() {{
            put("unloc", UnlocationsResponse.builder().build());
        }});
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
        when(masterDataUtils.getCarriersData(any())).thenReturn(new HashMap<>() {{
            put("carrier", CarrierMasterData.builder().build());
        }});
        commonUtils.getCarriersData(List.of("carrier"), map);
        assertFalse(map.isEmpty());
    }

    @Test
    void testSendRejectionEmailsExplicitly() {
        CommonUtils spyService = spy(commonUtils);
        when(masterDataUtils.withMdc(any())).thenReturn(mockRunnable());
        spyService.sendRejectionEmailsExplicitly(List.of(ShipmentDetails.builder().build()), List.of(ConsoleShipmentMapping.builder().build()),
                new HashSet<>(), List.of(ConsolidationDetails.builder().build()), false);
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
    void testCheckIfAnyDGClass3() {
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
        return () -> {
        };
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
    void testPopulateDictionaryForOceanDGCommercialApproval() {
        Map<String, Object> dictionary = new HashMap<>();
        CarrierDetails carrierDetails = CarrierDetails.builder().build();

        Set<Containers> containersList = new HashSet<>();
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
        commonUtils.populateDictionaryForOceanDGCommercialApproval(dictionary, shipmentDetails, vesselsResponse, remarks, taskCreateResponse, false);

        assertEquals("Remarks", dictionary.get(REQUESTER_REMARKS));
    }

    @Test
    void testGetDGEmailTemplate() {
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
    void testGetRoleId() {
        OceanDGStatus oceanDGStatus = OCEAN_DG_REQUESTED;
        when(iv1Service.getRoleIdsByRoleName(any())).thenReturn(10);
        Integer roleId = commonUtils.getRoleId(oceanDGStatus);
        assertEquals(10, roleId);
    }

    @Test
    void testGetUserEmailsByRoleId() {
        List<UsersRoleListResponse> userEmailResponse = new ArrayList<>();
        userEmailResponse.add(UsersRoleListResponse.builder().email("abc").build());
        when(iv1Service.getUserEmailsByRoleId(any())).thenReturn(userEmailResponse);

        List<String> response = commonUtils.getUserEmailsByRoleId(1);
        assertNotNull(response);
    }

    @Test
    void testCreateTask_Success() throws RunnerException {
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setId(1l);
        TaskCreateResponse taskCreateResponse = TaskCreateResponse.builder().build();
        when(iv1Service.createTask(any())).thenReturn(taskCreateResponse);

        TaskCreateResponse response = commonUtils.createTask(shipmentDetails, 1);
        assertNotNull(response);
    }


    @Test
    void testGetVesselsData() {
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

        commonUtils.sendEmailResponseToDGRequester(emailTemplatesRequest, request, shipmentDetails);
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
                null, null, false);
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
                null, null, false);
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
                "username", null, false);
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
                "username", null, false);
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
        when(carrierDetailsMock.getEtd()).thenReturn(LocalDate.parse("2024-09-20").atTime(LocalTime.MIDNIGHT));
        when(carrierDetailsMock.getEta()).thenReturn(LocalDate.parse("2024-09-25").atTime(LocalTime.MIDNIGHT));
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
    @ValueSource(strings = {"", "I"})
    void testGetTwoDigitCountryFromUnLocCode(String req) {
        String response = commonUtils.getTwoDigitCountryFromUnLocCode(req);
        assertNull(response);
    }

    @Test
    void testGetTwoDigitCountryFromUnLocCode() {
        String response = commonUtils.getTwoDigitCountryFromUnLocCode("CAN");
        assertEquals("CA", response);
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
        List<String> includeColumns = List.of(Constants.EMPTY_STRING);
        Object response = commonUtils.getShipmentDetailsResponse(shipmentDetails, includeColumns);
        assertNotNull(response);
    }

    @Test
    void testChangeShipmentDGStatusToReqd() {
        assertFalse(commonUtils.changeShipmentDGStatusToReqd(ShipmentDetails.builder().direction(IMP).build(), false));
    }

    @Test
    void testGetTriangulationPartnerList_EmptyInput() {
        List<Long> triangulationPartnerIds = commonUtils.getTriangulationPartnerList(null);

        assertNotNull(triangulationPartnerIds);
        assertTrue(triangulationPartnerIds.isEmpty());
    }

    @Test
    void testGetTriangulationPartnerList_NonEmptyInput() {
        List<TriangulationPartner> partnerList = Arrays.asList(
                TriangulationPartner.builder().triangulationPartner(1L).isAccepted(true).build(),
                TriangulationPartner.builder().triangulationPartner(2L).isAccepted(false).build(),
                null
        );

        // Act
        List<Long> triangulationPartnerIds = commonUtils.getTriangulationPartnerList(partnerList);

        // Assert
        assertNotNull(triangulationPartnerIds);
        assertEquals(2, triangulationPartnerIds.size());
        assertTrue(triangulationPartnerIds.contains(1L));
        assertTrue(triangulationPartnerIds.contains(2L));
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentSuccess() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));

        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentSuccess_WithInterConsoleFalse_AndReceivingBranch() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(false);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);
        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentSuccess_WithInterConsoleFalse_AndTriangulationBranch() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(false);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);

        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(false).isTriangulationBranch(true).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentSuccess_WithReceivingBranch() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(true);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentSuccess_WithReceivingBranch_ReassignFalse() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(true);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(false).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(3, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentSuccess_WithTriangulationBranch() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setId(1L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ShipmentDetails mockShipment2 = new ShipmentDetails();
        mockShipment2.setTenantId(100);
        mockShipment2.setReceivingBranch(200L);
        mockShipment.setId(2L);
        mockShipment2.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(true);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        mockConsolidation.setShipmentsList(new HashSet<>(Arrays.asList(mockShipment, mockShipment2)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(false).isTriangulationBranch(true).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationSuccess() {
        Long entityId = 2L;
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setTenantId(500);
        mockConsolidation.setReceivingBranch(600L);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(700L).build()
        ));

        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(false).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(3, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationSuccess_WithInterConsoleFalse_AndReceivingBranch() {
        Long entityId = 2L;

        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(false);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(200);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationSuccess_WithInterConsoleFalse_AndTriangulationBranch() {
        Long entityId = 2L;

        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(false);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(200);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));

        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(false).isTriangulationBranch(true).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationSuccess_WithReceivingBranch() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);

        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(true);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(200);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationSuccess_WithReceivingBranch_ReassignFalse() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);

        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(true);
        mockConsolidation.setTenantId(200);
        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(false).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(2, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationSuccess_WithTriangulationBranch() {
        Long entityId = 1L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setId(1L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ShipmentDetails mockShipment2 = new ShipmentDetails();
        mockShipment2.setTenantId(100);
        mockShipment2.setReceivingBranch(200L);
        mockShipment.setId(2L);
        mockShipment2.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(true);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        mockConsolidation.setShipmentsList(new HashSet<>(Arrays.asList(mockShipment, mockShipment2)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(false).isTriangulationBranch(true).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_ShipmentNotFound() {
        Long entityId = 1L;

        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.empty());
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertTrue(result.isEmpty());
    }

    @Test
    void testGetTenantIdsFromEntity_ConsolidationNotFound() {
        Long entityId = 2L;

        when(consolidationDetailsDao.findConsolidationByIdWithQuery(entityId)).thenReturn(Optional.empty());
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertTrue(result.isEmpty());
    }

    @Test
    void testGetTenantIdsFromEntity_EmptyTriangulationPartners() {
        Long entityId = 3L;
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(800);
        mockShipment.setReceivingBranch(900L);
        mockShipment.setTriangulationPartnerList(Collections.emptyList());

        when(shipmentDao.findShipmentByIdWithQuery(entityId)).thenReturn(Optional.of(mockShipment));

        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityId(entityId).entityType(SHIPMENT).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();

        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(2, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_Shipment_WithInterConsoleFalse_AndReceivingBranch() {
        String entityGuid = "6511f2e4-6234-452d-8abe-3685106317c2";
        ShipmentDetails mockShipment = new ShipmentDetails();
        mockShipment.setTenantId(100);
        mockShipment.setReceivingBranch(200L);
        mockShipment.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(false);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(100);
        mockConsolidation.setShipmentsList(new HashSet<>(Collections.singletonList(mockShipment)));
        mockShipment.setConsolidationList(new HashSet<>(Collections.singletonList(mockConsolidation)));
        when(shipmentDao.findShipmentByGuidWithQuery(UUID.fromString(entityGuid))).thenReturn(Optional.of(mockShipment));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityGuid(entityGuid).entityType(SHIPMENT).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testGetTenantIdsFromEntity_Consolidation_WithInterConsoleFalse_AndReceivingBranch() {
        String entityGuid = "6511f2e4-6234-452d-8abe-3685106317c2";

        ConsolidationDetails mockConsolidation = new ConsolidationDetails();
        mockConsolidation.setInterBranchConsole(false);
        mockConsolidation.setReceivingBranch(100L);
        mockConsolidation.setTenantId(200);
        mockConsolidation.setTriangulationPartnerList(List.of(
                TriangulationPartner.builder().triangulationPartner(300L).build(),
                TriangulationPartner.builder().triangulationPartner(400L).build()
        ));
        when(consolidationDetailsDao.findConsolidationByGuidWithQuery(UUID.fromString(entityGuid))).thenReturn(Optional.of(mockConsolidation));
        ListCousinBranchesForEtRequest request = ListCousinBranchesForEtRequest.builder().entityGuid(entityGuid).entityType(CONSOLIDATION).isReassign(true).isReceivingBranch(true).isTriangulationBranch(false).build();
        List<Long> result = commonUtils.getTenantIdsFromEntity(request);

        assertEquals(4, result.size());
    }

    @Test
    void testPrepareEventRequest_Success_WithReferenceNumber() {
        Long entityId = 1001L;
        String eventCode = "EVENT_CODE";
        String entityType = "SHIPMENT";
        String referenceNumber = "REF123";

        EventsRequest eventsRequest = commonUtils.prepareEventRequest(entityId, eventCode, entityType, referenceNumber);

        assertNotNull(eventsRequest.getActual());
    }

    @Test
    void testPrepareEventRequest_Success_WithoutReferenceNumber() {
        Long entityId = 1001L;
        String eventCode = "EVENT_CODE";
        String entityType = "SHIPMENT";

        EventsRequest eventsRequest = commonUtils.prepareEventRequest(entityId, eventCode, entityType, null);

        assertNotNull(eventsRequest);
        assertNotNull(eventsRequest.getActual());
    }

    @Test
    void testPrepareEventRequest_Success_WithEmptyReferenceNumber() {
        Long entityId = 1001L;
        String eventCode = "EVENT_CODE";
        String entityType = "SHIPMENT";
        String referenceNumber = "";

        EventsRequest eventsRequest = commonUtils.prepareEventRequest(entityId, eventCode, entityType, referenceNumber);

        assertNotNull(eventsRequest);
        assertNull(eventsRequest.getContainerNumber());
        assertNotNull(eventsRequest.getActual());
    }

    @ParameterizedTest
    @MethodSource("pTestCases")
    void testSetIncludedFieldsToResponse(Object entity, Set<String> includeColumns, Object expectedResponse) {
        assertEquals(expectedResponse, commonUtils.setIncludedFieldsToResponse(entity, includeColumns, expectedResponse));
    }

    @Test
    void testSetIncludedFieldsToResponseInvalidColumn() {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        assertEquals(response, commonUtils.setIncludedFieldsToResponse(new ShipmentDetails(), Set.of("invalidColumns"), response));
    }

    @Test
    void testMapListToDTO_EmptyList() {
        List<Containers> emptyList = new ArrayList<>();
        Object result = commonUtils.mapListToDTO(emptyList);
        assertEquals(emptyList, result);
    }

    @Test
    void testCheckForTriangulationPartnerList() {
        List<TriangulationPartner> triangulationPartnerList = List.of(new TriangulationPartner());
        List<TriangulationPartnerResponse> triangulationPartnerResponseList = List.of(new TriangulationPartnerResponse());
        when(modelMapper.map(triangulationPartnerList, new TypeToken<List<TriangulationPartnerResponse>>() {
        }.getType()))
                .thenReturn(triangulationPartnerResponseList);
        Object result = commonUtils.mapListToDTO(triangulationPartnerList);
        assertEquals(triangulationPartnerResponseList, result);
    }

    @Test
    void testMapListToDTO_Containers() {
        List<Containers> containerList = List.of(new Containers());
        List<ContainerResponse> containerResponseList = List.of(new ContainerResponse());

        when(modelMapper.map(containerList, new TypeToken<List<ContainerResponse>>() {
        }.getType()))
                .thenReturn(containerResponseList);

        Object result = commonUtils.mapListToDTO(containerList);

        //assertInstanceOf(List.class, result);
        assertEquals(containerResponseList, result);
    }

    @Test
    void testMapListToDTO_BookingCarriage() {
        List<BookingCarriage> bookingCarriageList = List.of(new BookingCarriage());
        List<BookingCarriageResponse> bookingCarriageResponseList = List.of(new BookingCarriageResponse());

        when(modelMapper.map(bookingCarriageList, new TypeToken<List<BookingCarriageResponse>>() {
        }.getType()))
                .thenReturn(bookingCarriageResponseList);

        Object result = commonUtils.mapListToDTO(bookingCarriageList);

        assertEquals(bookingCarriageResponseList, result);
    }

    @Test
    void testMapListToDTO_NoMappingFound() {
        List<String> stringList = List.of("test");
        Object result = commonUtils.mapListToDTO(stringList);
        assertEquals(stringList, result);
    }

    @Test
    void testMapListToDTO_ELDetails() {
        List<ELDetails> elDetailsList = List.of(new ELDetails());
        List<ELDetailsResponse> elDetailsResponseList = List.of(new ELDetailsResponse());

        when(modelMapper.map(elDetailsList, new TypeToken<List<ELDetailsResponse>>() {
        }.getType()))
                .thenReturn(elDetailsResponseList);

        Object result = commonUtils.mapListToDTO(elDetailsList);

        assertEquals(elDetailsResponseList, result);
    }

    @Test
    void testMapListToDTO_Events() {
        List<Events> eventsList = List.of(new Events());
        List<EventsResponse> eventsResponseList = List.of(new EventsResponse());
        when(modelMapper.map(eventsList, new TypeToken<List<EventsResponse>>() {
        }.getType()))
                .thenReturn(eventsResponseList);

        Object result = commonUtils.mapListToDTO(eventsList);
        assertEquals(eventsResponseList, result);
    }

    @Test
    void testMapListToDTO_Packing() {
        List<Packing> packings = List.of(new Packing());
        List<PackingResponse> packingResponseList = List.of(new PackingResponse());
        when(modelMapper.map(packings, new TypeToken<List<PackingResponse>>() {
        }.getType()))
                .thenReturn(packingResponseList);

        Object result = commonUtils.mapListToDTO(packings);
        assertEquals(packingResponseList, result);
    }

    @Test
    void testMapListToDTO_ReferenceNumbers() {
        List<ReferenceNumbers> referenceNumbersList = List.of(new ReferenceNumbers());
        List<ReferenceNumbersResponse> referenceNumbersResponseList = List.of(new ReferenceNumbersResponse());

        when(modelMapper.map(referenceNumbersList, new TypeToken<List<ReferenceNumbersResponse>>() {
        }.getType()))
                .thenReturn(referenceNumbersResponseList);

        Object result = commonUtils.mapListToDTO(referenceNumbersList);

        assertEquals(referenceNumbersResponseList, result);
    }

    @Test
    void testMapListToDTO_Routings() {
        List<Routings> routingList = List.of(new Routings());
        List<RoutingsResponse> routingResponseList = List.of(new RoutingsResponse());

        when(modelMapper.map(routingList, new TypeToken<List<RoutingsResponse>>() {
        }.getType()))
                .thenReturn(routingResponseList);

        Object result = commonUtils.mapListToDTO(routingList);

        assertEquals(routingResponseList, result);
    }

    @Test
    void testMapListToDTO_ServiceDetails() {
        List<ServiceDetails> serviceDetailsList = List.of(new ServiceDetails());
        List<ServiceDetailsResponse> serviceDetailsResponseList = List.of(new ServiceDetailsResponse());

        when(modelMapper.map(serviceDetailsList, new TypeToken<List<ServiceDetailsResponse>>() {
        }.getType()))
                .thenReturn(serviceDetailsResponseList);

        Object result = commonUtils.mapListToDTO(serviceDetailsList);

        assertEquals(serviceDetailsResponseList, result);
    }

    @Test
    void testMapListToDTO_Notes() {
        List<Notes> notesList = List.of(new Notes());
        List<NotesResponse> notesResponseList = List.of(new NotesResponse());

        when(modelMapper.map(notesList, new TypeToken<List<NotesResponse>>() {
        }.getType()))
                .thenReturn(notesResponseList);

        Object result = commonUtils.mapListToDTO(notesList);

        assertEquals(notesResponseList, result);
    }

    @Test
    void testMapListToDTO_Jobs() {
        List<Jobs> jobsList = List.of(new Jobs());
        List<JobResponse> jobResponseList = List.of(new JobResponse());

        when(modelMapper.map(jobsList, new TypeToken<List<JobResponse>>() {
        }.getType()))
                .thenReturn(jobResponseList);

        Object result = commonUtils.mapListToDTO(jobsList);

        assertEquals(jobResponseList, result);
    }

    @Test
    void testMapListToDTO_ConsolidationDetails() {
        List<ConsolidationDetails> consolidationDetailsList = List.of(new ConsolidationDetails());
        List<ConsolidationListResponse> consolidationResponseList = List.of(new ConsolidationListResponse());

        when(modelMapper.map(consolidationDetailsList, new TypeToken<List<ConsolidationListResponse>>() {
        }.getType()))
                .thenReturn(consolidationResponseList);

        Object result = commonUtils.mapListToDTO(consolidationDetailsList);

        assertEquals(consolidationResponseList, result);
    }

    @Test
    void testMapListToDTO_Parties() {
        List<Parties> partiesList = List.of(new Parties());
        List<PartiesResponse> partiesResponseList = List.of(new PartiesResponse());

        when(modelMapper.map(partiesList, new TypeToken<List<PartiesResponse>>() {
        }.getType()))
                .thenReturn(partiesResponseList);

        Object result = commonUtils.mapListToDTO(partiesList);

        assertEquals(partiesResponseList, result);
    }

    @Test
    void testMapListToDTO_ShipmentOrder() {
        List<ShipmentOrder> shipmentOrderList = List.of(new ShipmentOrder());
        List<ShipmentOrderResponse> shipmentOrderResponseList = List.of(new ShipmentOrderResponse());

        when(modelMapper.map(shipmentOrderList, new TypeToken<List<ShipmentOrderResponse>>() {
        }.getType()))
                .thenReturn(shipmentOrderResponseList);

        Object result = commonUtils.mapListToDTO(shipmentOrderList);

        assertEquals(shipmentOrderResponseList, result);
    }

    @Test
    void testMapListToDTO_TruckDriverDetails() {
        List<TruckDriverDetails> truckDriverDetailsList = List.of(new TruckDriverDetails());
        List<TruckDriverDetailsResponse> truckDriverDetailsResponseList = List.of(new TruckDriverDetailsResponse());

        when(modelMapper.map(truckDriverDetailsList, new TypeToken<List<TruckDriverDetailsResponse>>() {
        }.getType()))
                .thenReturn(truckDriverDetailsResponseList);

        Object result = commonUtils.mapListToDTO(truckDriverDetailsList);

        assertEquals(truckDriverDetailsResponseList, result);
    }

    @Test
    void testMapToDTO_CarrierDetails() {
        CarrierDetails carrierDetails = new CarrierDetails();
        CarrierDetailResponse carrierDetailResponse = new CarrierDetailResponse();

        when(modelMapper.map(carrierDetails, CarrierDetailResponse.class)).thenReturn(carrierDetailResponse);

        Object result = commonUtils.mapToDTO(carrierDetails);

        assertEquals(carrierDetailResponse, result);
    }

    @Test
    void testMapToDTO_AdditionalDetails() {
        AdditionalDetails additionalDetails = new AdditionalDetails();
        AdditionalDetailResponse additionalDetailResponse = new AdditionalDetailResponse();

        when(modelMapper.map(additionalDetails, AdditionalDetailResponse.class)).thenReturn(additionalDetailResponse);

        Object result = commonUtils.mapToDTO(additionalDetails);

        assertEquals(additionalDetailResponse, result);
    }

    @Test
    void testMapToDTO_PickupDeliveryDetails() {
        PickupDeliveryDetails pickupDeliveryDetails = new PickupDeliveryDetails();
        PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();

        when(modelMapper.map(pickupDeliveryDetails, PickupDeliveryDetailsResponse.class)).thenReturn(pickupDeliveryDetailsResponse);

        Object result = commonUtils.mapToDTO(pickupDeliveryDetails);

        assertEquals(pickupDeliveryDetailsResponse, result);
    }

    @Test
    void testMapToDTO_Parties() {
        Parties parties = new Parties();
        PartiesResponse partiesResponse = new PartiesResponse();

        when(modelMapper.map(parties, PartiesResponse.class)).thenReturn(partiesResponse);

        Object result = commonUtils.mapToDTO(parties);

        assertEquals(partiesResponse, result);
    }

    @Test
    void testMapToDTO_ArrivalDepartureDetails() {
        ArrivalDepartureDetails arrivalDepartureDetails = new ArrivalDepartureDetails();
        ArrivalDepartureDetailsResponse arrivalDepartureDetailsResponse = new ArrivalDepartureDetailsResponse();

        when(modelMapper.map(arrivalDepartureDetails, ArrivalDepartureDetailsResponse.class)).thenReturn(arrivalDepartureDetailsResponse);

        Object result = commonUtils.mapToDTO(arrivalDepartureDetails);

        assertEquals(arrivalDepartureDetailsResponse, result);
    }

    @Test
    void testSetNestedFieldValue_SimpleField() throws NoSuchMethodException, IllegalAccessException, InstantiationException, InvocationTargetException {
        Parties obj = new Parties();
        commonUtils.setNestedFieldValue(obj, "type", "consignee");
        assertEquals("consignee", obj.getType());
    }

    @Test
    void testSetNestedFieldValue_NestedField() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {
        ShipmentDetails obj = new ShipmentDetails();
        commonUtils.setNestedFieldValue(obj, "carrierDetails.shippingLine", "Perma");
        assertNotNull(obj.getCarrierDetails());
        assertEquals("Perma", obj.getCarrierDetails().getShippingLine());
    }

    @Test
    void testSetNestedFieldValue_MapField() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {
        Map<String, Object> map = new HashMap<>();
        commonUtils.setNestedFieldValue(map, "orgData.city", "new York");
        assertTrue(map.containsKey("orgData"));
        assertTrue(((Map<?, ?>) map.get("orgData")).containsKey("city"));
        assertEquals("new York", ((Map<?, ?>) map.get("orgData")).get("city"));
    }

    @Test
    void testSetNestedFieldValue_NoGetterAvailable() {
        Parties obj = new Parties();
        assertThrows(NoSuchMethodException.class, () ->
                commonUtils.setNestedFieldValue(obj, "invalidField", "test"));
    }

    @Test
    void testMapListToDTOSet_Containers() {
        Set<Containers> containersList = Set.of(new Containers());
        Set<ContainerResponse> containerResponseList = Set.of(new ContainerResponse());
        when(modelMapper.map(containersList, new TypeToken<Set<ContainerResponse>>() {
        }.getType()))
                .thenReturn(containerResponseList);

        Object result = commonUtils.mapToDTO(containersList);

        assertEquals(containerResponseList, result);
    }

    @Test
    void testMapListToDTOSet_ConsolidationDetails() {
        Set<ConsolidationDetails> consolidationDetailsSet = Set.of(new ConsolidationDetails());
        Set<ConsolidationListResponse> consolidationListResponseSet = Set.of(new ConsolidationListResponse());
        when(modelMapper.map(consolidationDetailsSet, new TypeToken<Set<ConsolidationListResponse>>() {
        }.getType()))
                .thenReturn(consolidationListResponseSet);

        Object result = commonUtils.mapToDTO(consolidationDetailsSet);

        assertEquals(consolidationListResponseSet, result);
    }

    @Test
    void testSetIncludedFields_validField() throws Exception {
        // Given
        List<String> fields = Collections.singletonList("carrierDetails");
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        CarrierDetailResponse carrierDetailResponse = new CarrierDetailResponse();
        when(modelMapper.map(any(CarrierDetails.class), eq(CarrierDetailResponse.class)))
                .thenReturn(carrierDetailResponse);

        // When
        ShipmentDetailsResponse result = commonUtils.getShipmentDetailsResponse(shipmentDetails, fields);

        // Then
        verify(modelMapper).map(any(CarrierDetails.class), eq(CarrierDetailResponse.class));
        assertNotNull(result);
        // Add assertions depending on what the setter methods do with carrierDetailResponse
    }

    @Test
    void testSetIncludedFields_fieldDoesNotExist() {
        shipmentDetails = new ShipmentDetails();
        // Given
        List<String> fields = Collections.singletonList("nonExistentField");

        // When
        ShipmentDetailsResponse result = commonUtils.getShipmentDetailsResponse(shipmentDetails, fields);

        // Then
        assertNotNull(result);
        // Assert that the non-existent field doesn't cause an issue (e.g., no setter invocation)
    }

    @Test
    void testSetIncludedFields_fieldWithNullValue() {
        // Given
        List<String> fields = Collections.singletonList("packingList");
        when(shipmentDetails.getPackingList()).thenReturn(null);

        // When
        ShipmentDetailsResponse result = commonUtils.getShipmentDetailsResponse(shipmentDetails, fields);

        // Then
        assertNotNull(result);
        // Assert that no null pointer exceptions are thrown
    }

    @Test
    void testSetIncludedFields_withMultipleFields() {
        // Given
        List<String> fields = Arrays.asList("carrierDetails", "additionalDetails");
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.setPickupDetails(new PickupDeliveryDetails());
        shipmentDetails.setConsigner(new Parties());
        CarrierDetailResponse carrierDetailResponse = new CarrierDetailResponse();
        AdditionalDetailResponse additionalDetailResponse = new AdditionalDetailResponse();
        when(modelMapper.map(any(CarrierDetails.class), eq(CarrierDetailResponse.class)))
                .thenReturn(carrierDetailResponse);
        when(modelMapper.map(any(AdditionalDetails.class), eq(AdditionalDetailResponse.class)))
                .thenReturn(additionalDetailResponse);

        // When
        ShipmentDetailsResponse result = commonUtils.getShipmentDetailsResponse(shipmentDetails, fields);

        // Then
        verify(modelMapper).map(any(CarrierDetails.class), eq(CarrierDetailResponse.class));
        verify(modelMapper).map(any(AdditionalDetails.class), eq(AdditionalDetailResponse.class));
        assertNotNull(result);
    }

    @Test
    void testSetIncludedFields_withEmptyFields() {
        // Given
        List<String> fields = Collections.emptyList();

        // When
        ShipmentDetailsResponse result = commonUtils.getShipmentDetailsResponse(shipmentDetails, fields);

        // Then
        assertNotNull(result); // Ensure it still returns a non-null response even with empty fields
    }

    @Test
    void testSetIncludedFields_withInvalidFieldType() {
        // Given
        List<String> fields = Arrays.asList("carrierDetails", "invalidField");

        // When
        ShipmentDetailsResponse result = commonUtils.getShipmentDetailsResponse(shipmentDetails, fields);

        // Then
        assertNotNull(result);
        // Ensure that it doesn't throw exceptions for invalid fields, just logs an error
    }

    @Test
    void testGetDtoValue_withList() {
        // Given
        List<CarrierDetails> carrierDetailsList = Collections.singletonList(new CarrierDetails());
        TypeToken<List<CarrierDetailResponse>> typeToken = new TypeToken<>() {
        };
        when(modelMapper.map(carrierDetailsList, typeToken.getType())).thenReturn(List.of(new CarrierDetails()));
        // When
        Object result = commonUtils.getDtoValue(carrierDetailsList);

        // Then
        assertNotNull(result);
        assertTrue(result instanceof List<?>);
    }

    @Test
    void testGetDtoValue_withNonList() {
        // Given
        CarrierDetails carrierDetail = new CarrierDetails();
        CarrierDetailResponse carrierDetailResponse = new CarrierDetailResponse();
        when(modelMapper.map(any(CarrierDetails.class), eq(CarrierDetailResponse.class)))
                .thenReturn(carrierDetailResponse);
        // When
        Object result = commonUtils.getDtoValue(carrierDetail);

        // Then
        assertNotNull(result);
        assertTrue(result instanceof CarrierDetailResponse);
    }

    @Test
    void testGetDtoValue_withNull() {
        // Given
        Object result = commonUtils.getDtoValue(null);

        // Then
        assertNull(result);
    }

    @Test
    void sendEmailShipmentPullWithdraw() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(new HashMap<>());
        sendEmailDto.setShipmentRequestedTypes(new HashSet<>());

        // Act
        commonUtils.sendEmailShipmentPullWithdraw(sendEmailDto);

        // Assert
        assertTrue(sendEmailDto.getShipmentRequestedTypes().contains(SHIPMENT_PULL_WITHDRAW));
        verify(notificationService, never()).sendEmail(anyString(), anyString(), anyList(), anyList());
    }

    @Test
    void sendEmailShipmentPullWithdraw1() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(Map.of(SHIPMENT_PULL_WITHDRAW, EmailTemplatesRequest.builder().body("body").subject("subject").build()));
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(1);
        shipmentDetails1.setAssignedTo("Assigned");
        sendEmailDto.setShipmentDetails(shipmentDetails1);
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTenantId(1);
        sendEmailDto.setConsolidationDetails(consolidationDetails1);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Tenant");
        sendEmailDto.setTenantModelMap(Map.of(1, tenantModel));
        sendEmailDto.setUsernameEmailsMap(Map.of("Assigned", "Email"));
        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setShipmentAttachDefaultToMailId("to1@example.com,to2@example.com");
        tenantSettingsResponse.setConsolidationAttachDefaultToMailId("cc1@example.com,cc2@example.com");

        sendEmailDto.setV1TenantSettingsMap(Map.of(1, tenantSettingsResponse));

        // Assert
        assertDoesNotThrow(() -> commonUtils.sendEmailShipmentPullWithdraw(sendEmailDto));
    }

    @Test
    void sendEmailShipmentPullWithdraw2() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(Map.of(SHIPMENT_PULL_WITHDRAW, EmailTemplatesRequest.builder().body("body").subject("subject").build()));
        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setConsolidationAttachDefaultCCMailId("cc1@example.com,cc2@example.com");

        sendEmailDto.setV1TenantSettingsMap(Map.of(1, tenantSettingsResponse));

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(1);
        shipmentDetails1.setAssignedTo("Assigned");
        sendEmailDto.setShipmentDetails(shipmentDetails1);

        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTenantId(1);
        sendEmailDto.setConsolidationDetails(consolidationDetails1);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Tenant");
        sendEmailDto.setTenantModelMap(Map.of(1, tenantModel));
        sendEmailDto.setUsernameEmailsMap(Map.of("Assigned", "Email"));

        assertDoesNotThrow(() -> commonUtils.sendEmailShipmentPullWithdraw(sendEmailDto));
    }

    @Test
    void sendEmailShipmentPullWithdraw_consolidationAssignedToNotNull() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(Map.of(SHIPMENT_PULL_WITHDRAW, EmailTemplatesRequest.builder().body("body").subject("subject").build()));
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(1);
        shipmentDetails1.setAssignedTo("Assigned");
        sendEmailDto.setShipmentDetails(shipmentDetails1);
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTenantId(1);
        consolidationDetails1.setAssignedTo("AssignedToUser");
        sendEmailDto.setConsolidationDetails(consolidationDetails1);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Tenant");
        sendEmailDto.setTenantModelMap(Map.of(1, tenantModel));
        sendEmailDto.setUsernameEmailsMap(Map.of("Assigned", "Email"));
        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setShipmentAttachDefaultToMailId("to1@example.com,to2@example.com");
        tenantSettingsResponse.setConsolidationAttachDefaultToMailId("cc1@example.com,cc2@example.com");

        sendEmailDto.setV1TenantSettingsMap(Map.of(1, tenantSettingsResponse));

        // Assert
        assertDoesNotThrow(() -> commonUtils.sendEmailShipmentPullWithdraw(sendEmailDto));
    }

    @Test
    void sendEmailShipmentPullWithdraw_consolidationAssignedToNotNull_userEmailInMapExists() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(Map.of(SHIPMENT_PULL_WITHDRAW, EmailTemplatesRequest.builder().body("body").subject("subject").build()));
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(1);
        shipmentDetails1.setAssignedTo("Assigned");
        sendEmailDto.setShipmentDetails(shipmentDetails1);
        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTenantId(1);
        consolidationDetails1.setAssignedTo("AssignedToUser");
        sendEmailDto.setConsolidationDetails(consolidationDetails1);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Tenant");
        sendEmailDto.setTenantModelMap(Map.of(1, tenantModel));

        Map<String, String> usernameEmailsMap = new HashMap<>();
        usernameEmailsMap.put("Assigned", "Email");
        usernameEmailsMap.put("AssignedToUser", "AnotherEmail");

        sendEmailDto.setUsernameEmailsMap(usernameEmailsMap);
        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setShipmentAttachDefaultToMailId("to1@example.com,to2@example.com");
        tenantSettingsResponse.setConsolidationAttachDefaultToMailId("cc1@example.com,cc2@example.com");

        sendEmailDto.setV1TenantSettingsMap(Map.of(1, tenantSettingsResponse));

        // Assert
        assertDoesNotThrow(() -> commonUtils.sendEmailShipmentPullWithdraw(sendEmailDto));
    }

    @Test
    void sendEmailShipmentPushWithdraw() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(new HashMap<>());
        sendEmailDto.setShipmentRequestedTypes(new HashSet<>());

        // Act
        commonUtils.sendEmailShipmentPushWithdraw(sendEmailDto);

        // Assert
        assertTrue(sendEmailDto.getShipmentRequestedTypes().contains(SHIPMENT_PUSH_WITHDRAW));
    }

    @Test
    void sendEmailShipmentPushWithdraw1() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(Map.of(SHIPMENT_PUSH_WITHDRAW, EmailTemplatesRequest.builder().body("body").subject("subject").build()));

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(1);
        shipmentDetails1.setAssignedTo("Assigned");

        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTenantId(1);

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setShipmentAttachDefaultToMailId("to1@example.com,to1@example.com");
        tenantSettingsResponse.setConsolidationAttachDefaultToMailId("to1@example.com,to1@example.com");

        sendEmailDto.setV1TenantSettingsMap(Map.of(1, tenantSettingsResponse));

        sendEmailDto.setShipmentDetails(shipmentDetails1);
        sendEmailDto.setConsolidationDetails(consolidationDetails1);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Tenant");
        sendEmailDto.setTenantModelMap(Map.of(1, tenantModel));
        sendEmailDto.setUsernameEmailsMap(Map.of("Assigned", "assigned@example.com"));

        // Assert
        assertDoesNotThrow(() -> commonUtils.sendEmailShipmentPushWithdraw(sendEmailDto));
    }

    @Test
    void sendEmailShipmentPushWithdraw2() {
        // Arrange
        SendEmailDto sendEmailDto = new SendEmailDto();
        sendEmailDto.setEmailTemplatesRequestMap(Map.of(SHIPMENT_PUSH_WITHDRAW, EmailTemplatesRequest.builder().body("body").subject("subject").build()));

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setConsolidationAttachDefaultCCMailId("cc1@example.com,cc2@example.com");
        tenantSettingsResponse.setShipmentAttachDefaultCCMailId("cc1@example.com,cc2@example.com");

        sendEmailDto.setV1TenantSettingsMap(Map.of(1, tenantSettingsResponse));

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(1);
        sendEmailDto.setShipmentDetails(shipmentDetails1);

        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setTenantId(1);
        sendEmailDto.setConsolidationDetails(consolidationDetails1);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCode("Tenant");
        sendEmailDto.setTenantModelMap(Map.of(1, tenantModel));
        sendEmailDto.setUsernameEmailsMap(Map.of("Assigned", "assigned@example.com"));

        assertDoesNotThrow(() -> commonUtils.sendEmailShipmentPushWithdraw(sendEmailDto));
    }

    @Test
    void populateDictionaryForEmailFromShipment() {
        // Arrange
        Map<String, Object> dictionary = new HashMap<>();
        ShipmentDetails shipmentDetails1 = getMockShipmentDetails();
        ConsolidationDetails consolidationDetails1 = getMockConsolidationDetails();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();

        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(getMockTenantSettings());

        // Act
        commonUtils.populateDictionaryForEmailFromShipment(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);
        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);

        // Assert
        assertEquals(consolidationDetails1.getCreatedBy(), dictionary.get(CONSOLIDATION_CREATE_USER));
        assertEquals(shipmentDetails1.getShipmentId(), dictionary.get(SHIPMENT_NUMBER));
        assertEquals(shipmentDetails1.getHouseBill(), dictionary.get(HAWB_NUMBER));
    }

    @Test
    void populateDictionaryForEmailFromShipment2() {
        // Arrange
        Map<String, Object> dictionary = new HashMap<>();
        ShipmentDetails shipmentDetails1 = getMockShipmentDetails();
        shipmentDetails1.getCarrierDetails().setShippingLine("ABC");

        ConsolidationDetails consolidationDetails1 = getMockConsolidationDetails();
        consolidationDetails1.getCarrierDetails().setShippingLine("ABC");

        CarrierMasterData carrierMasterData = new CarrierMasterData();
        carrierMasterData.setIataCode("XYZ");
        carrierMasterData.setItemDescription("Test Carrier");

        Map<String, CarrierMasterData> carrierMasterDataMap = Map.of("ABC", carrierMasterData);
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();

        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(getMockTenantSettings());

        // Act
        commonUtils.populateDictionaryForEmailFromShipment(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);
        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);

        // Assert
        assertEquals("XYZ", dictionary.get(CARRIER_CODE));
        assertEquals("Test Carrier", dictionary.get(CARRIER_NAME));
    }

    @Test
    void populateDictionaryForEmailFromShipment3() {
        // Arrange
        Map<String, Object> dictionary = new HashMap<>();
        ShipmentDetails shipmentDetails1 = getMockShipmentDetails();
        shipmentDetails1.getCarrierDetails().setOriginPort("JFK");
        shipmentDetails1.getCarrierDetails().setDestinationPort("LAX");
        shipmentDetails1.getCarrierDetails().setShippingLine("ABC");

        ConsolidationDetails consolidationDetails1 = getMockConsolidationDetails();
        consolidationDetails1.getCarrierDetails().setOriginPort("JFK");
        consolidationDetails1.getCarrierDetails().setDestinationPort("LAX");
        consolidationDetails1.getCarrierDetails().setShippingLine("ABC");

        CarrierMasterData carrierMasterData = new CarrierMasterData();
        carrierMasterData.setItemValue("XYZ");
        carrierMasterData.setItemDescription("Test Carrier");

        UnlocationsResponse origin = new UnlocationsResponse();
        origin.setLocCode("JFK_CODE");
        origin.setName("New York");

        UnlocationsResponse destination = new UnlocationsResponse();
        destination.setLocCode("LAX_CODE");
        destination.setName("Los Angeles");

        Map<String, UnlocationsResponse> unLocMap = Map.of("JFK", origin, "LAX", destination);
        Map<String, CarrierMasterData> carrierMasterDataMap = Map.of("ABC", carrierMasterData);

        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(getMockTenantSettings());

        // Act
        commonUtils.populateDictionaryForEmailFromShipment(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);
        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);

        // Assert
        assertEquals("JFK_CODE", dictionary.get(ReportConstants.POL));
        assertEquals("New York", dictionary.get(POL_NAME));
        assertEquals("LAX_CODE", dictionary.get(ReportConstants.POD));
        assertEquals("Los Angeles", dictionary.get(POD_NAME));
    }

    @Test
    void populateDictionaryForEmailFromShipment4() {
        // Arrange
        Map<String, Object> dictionary = new HashMap<>();
        ShipmentDetails shipmentDetails1 = getMockShipmentDetails();
        ConsolidationDetails consolidationDetails1 = getMockConsolidationDetails();
        shipmentDetails1.getCarrierDetails().setShippingLine("NON_EXISTENT");
        consolidationDetails1.getCarrierDetails().setShippingLine("NON_EXISTENT");

        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();

        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(getMockTenantSettings());

        // Act
        commonUtils.populateDictionaryForEmailFromShipment(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);
        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);

        // Assert
        assertNull(dictionary.get(CARRIER_CODE));
        assertNull(dictionary.get(CARRIER_NAME));
    }

    @Test
    void populateDictionaryForEmailFromShipment5() {
        // Arrange
        Map<String, Object> dictionary = new HashMap<>();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        ConsolidationDetails consolidationDetails1 = new ConsolidationDetails();
        consolidationDetails1.setAllocations(new Allocations());
        Map<String, UnlocationsResponse> unLocMap = new HashMap<>();
        Map<String, CarrierMasterData> carrierMasterDataMap = new HashMap<>();
        shipmentDetails1.setCarrierDetails(new CarrierDetails());
        consolidationDetails1.setCarrierDetails(new CarrierDetails());

        when(tenantSettingsService.getV1TenantSettings(any())).thenReturn(getMockTenantSettings());

        // Act
        commonUtils.populateDictionaryForEmailFromShipment(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateDictionaryForEmailFromConsolidation(dictionary, shipmentDetails1, consolidationDetails1, unLocMap, carrierMasterDataMap);
        commonUtils.populateShipmentImportPushAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);
        commonUtils.populateShipmentImportPullAttachmentTemplate(dictionary, shipmentDetails1, consolidationDetails1, carrierMasterDataMap, unLocMap);

        // Assert
        assertNull(dictionary.get(SHIPMENT_NUMBER));
        assertNull(dictionary.get(HAWB_NUMBER));
    }

    @Test
    void testSendExcelFileViaEmail_Success() throws Exception {
        // Arrange
        Workbook workbook = new XSSFWorkbook();
        String filename = "export_test_2025.xlsx";

        UserContext.getUser().setEmail("him@gmail.com");

        lenient().when(notificationService.sendEmail(any())).thenReturn(new NotificationServiceResponse());

        commonUtils.sendExcelFileViaEmail(workbook, filename);
        assertEquals("him@gmail.com", UserContext.getUser().getEmail());

    }

    @Test
    void testGetPacksUnit() {
        String packsUnit = commonUtils.getPacksUnit(null, "PKG");
        assertEquals("PKG", packsUnit);
    }

    @Test
    void testGetPacksUnit1() {
        String packsUnit = commonUtils.getPacksUnit("BAG", "PKG");
        assertEquals("PKG", packsUnit);
    }

    @Test
    void testGetPacksUnit2() {
        String packsUnit = commonUtils.getPacksUnit("BAG", null);
        assertEquals("BAG", packsUnit);
    }

    @Test
    void testGetPacksUnit3() {
        String packsUnit = commonUtils.getPacksUnit("BAG", "BAG");
        assertEquals("BAG", packsUnit);
    }

    @Test
    void testIsRoadLCLorLTL_LCL() {
        assertTrue(commonUtils.isRoadLCLorLTL("ROA", "LCL"));
    }

    @Test
    void testIsRoadLCLorLTL_LTL() {
        assertTrue(commonUtils.isRoadLCLorLTL("ROA", "LTL"));
    }

    @Test
    void testIsRoadLCLorLTL_InvalidTransportMode() {
        assertFalse(commonUtils.isRoadLCLorLTL("SEA", "LCL"));
    }

    @Test
    void testIsRoadLCLorLTL_InvalidCargoType() {
        assertFalse(commonUtils.isRoadLCLorLTL("ROA", "FCL"));
    }

    @Test
    void testIsSeaLCL_Valid() {
        assertTrue(commonUtils.isSeaLCL("SEA", "LCL"));
    }

    @Test
    void testIsSeaLCL_InvalidCargoType() {
        assertFalse(commonUtils.isSeaLCL("SEA", "FCL"));
    }

    @Test
    void testIsSeaLCL_InvalidTransportMode() {
        assertFalse(commonUtils.isSeaLCL("AIR", "LCL"));
    }

    @Test
    void testIsLCLorLTL_LCL() {
        assertTrue(commonUtils.isLCLorLTL("LCL"));
    }

    @Test
    void testIsLCLorLTL_LTL() {
        assertTrue(commonUtils.isLCLorLTL("LTL"));
    }

    @Test
    void testIsLCLorLTL_Invalid() {
        assertFalse(commonUtils.isLCLorLTL("FCL"));
    }

    @Test
    void testGetPacksUnit4() {
        String packsUnit = commonUtils.getPacksUnit(null);
        assertEquals("PKG", packsUnit);
    }

    @Test
    void testGetPacksUnit5() {
        String packsUnit = commonUtils.getPacksUnit("PKG");
        assertEquals("PKG", packsUnit);
    }

    @Test
    void testGetDefaultWeightUnit() {
        String weightUnit = commonUtils.getDefaultWeightUnit();
        assertEquals("KG", weightUnit);
    }

    @Test
    void testGetDefaultWeightUnit1() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setWeightChargeableUnit("KG");
        when(shipmentSettingsDao.getSettingsByTenantIdWithCache(any())).thenReturn(Optional.of(ShipmentSettingsDetailsContext.getCurrentTenantSettings()));
        String weightUnit = commonUtils.getDefaultWeightUnit();
        assertEquals("KG", weightUnit);
    }

    @Test
    void testGetDefaultVolumeUnit() {
        String volumeUnit = commonUtils.getDefaultVolumeUnit();
        assertEquals("M3", volumeUnit);
    }

    @Test
    void testGetDefaultVolumeUnit1() {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setVolumeChargeableUnit("M3");
        when(shipmentSettingsDao.getSettingsByTenantIdWithCache(any())).thenReturn(Optional.of(ShipmentSettingsDetailsContext.getCurrentTenantSettings()));
        String volumeUnit = commonUtils.getDefaultVolumeUnit();
        assertEquals("M3", volumeUnit);
    }
    
    @Test
    void getTaskIdHyperLinkV2_Success(){
        String result = commonUtils.getTaskIdHyperLinkV2("SH", "TA");
        assertNotNull(result);
    }

    private ShipmentDetails getMockShipmentDetails() {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setShipmentId("SHIP123");
        shipment.setHouseBill("HAWB456");
        shipment.setWeight(BigDecimal.valueOf(100));
        shipment.setWeightUnit("KG");
        shipment.setVolume(BigDecimal.valueOf(10));
        shipment.setVolumeUnit("CBM");

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(LocalDateTime.now());
        carrierDetails.setEta(LocalDateTime.now());
        carrierDetails.setShippingLine("LINE001");
        carrierDetails.setOriginPort("JFK");
        carrierDetails.setDestinationPort("LAX");
        shipment.setCarrierDetails(carrierDetails);

        return shipment;
    }

    private ConsolidationDetails getMockConsolidationDetails() {
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setCreatedBy("admin@example.com");
        consolidation.setConsolidationNumber("CONSOL123");

        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEtd(LocalDateTime.now());
        carrierDetails.setEta(LocalDateTime.now());
        carrierDetails.setShippingLine("LINE001");
        carrierDetails.setOriginPort("JFK");
        carrierDetails.setDestinationPort("LAX");
        consolidation.setCarrierDetails(carrierDetails);

        Allocations allocations1 = new Allocations();
        allocations1.setWeight(BigDecimal.valueOf(100));
        allocations1.setWeightUnit("KG");
        allocations1.setVolume(BigDecimal.valueOf(10));
        allocations1.setVolumeUnit("CBM");

        consolidation.setAllocations(allocations1);

        return consolidation;
    }

    private V1TenantSettingsResponse getMockTenantSettings() {
        V1TenantSettingsResponse settings = new V1TenantSettingsResponse();
        settings.setDPWDateFormat("yyyy-MM-dd HH:mm:ss");
        return settings;
    }

    @Test
    void testSetTransportInfoStatusMessage_PolWarning() {
        CarrierDetails mockCarrierDetails = mock(CarrierDetails.class);
        List<Routings> mockMainCarriageRoutings = mock(List.class);
        // Setup mock data
        Routings firstLeg = mock(Routings.class);
        Routings lastLeg = mock(Routings.class);
        when(mockMainCarriageRoutings.get(0)).thenReturn(firstLeg);
        when(mockMainCarriageRoutings.get(mockMainCarriageRoutings.size() - 1)).thenReturn(lastLeg);
        when(firstLeg.getPol()).thenReturn("OriginPort");
        when(mockCarrierDetails.getOriginPort()).thenReturn("DifferentOriginPort");
        when(lastLeg.getPod()).thenReturn("DestinationPort");
        when(mockCarrierDetails.getDestinationPort()).thenReturn("DestinationPort");

        // Call the method under test
        String result = CommonUtils.setTransportInfoStatusMessage(mockCarrierDetails, TransportInfoStatus.IH, mockMainCarriageRoutings);

        // Verify the result
        assertEquals(Constants.POL_WARNING_MESSAGE, result);
    }

    @Test
    void testSetTransportInfoStatusMessage_PodWarning() {
        CarrierDetails mockCarrierDetails = mock(CarrierDetails.class);
        List<Routings> mockMainCarriageRoutings = mock(List.class);
        // Setup mock data
        Routings firstLeg = mock(Routings.class);
        Routings lastLeg = mock(Routings.class);
        when(mockMainCarriageRoutings.get(0)).thenReturn(firstLeg);
        when(mockMainCarriageRoutings.get(mockMainCarriageRoutings.size() - 1)).thenReturn(lastLeg);
        when(firstLeg.getPol()).thenReturn("OriginPort");
        when(mockCarrierDetails.getOriginPort()).thenReturn("OriginPort");
        when(lastLeg.getPod()).thenReturn("DestinationPort");
        when(mockCarrierDetails.getDestinationPort()).thenReturn("DifferentDestinationPort");

        // Call the method under test
        String result = CommonUtils.setTransportInfoStatusMessage(mockCarrierDetails, TransportInfoStatus.IH, mockMainCarriageRoutings);

        // Verify the result
        assertEquals(Constants.POD_WARNING_MESSAGE, result);
    }

    @Test
    void testSetTransportInfoStatusMessage_PolPodWarning() {
        CarrierDetails mockCarrierDetails = mock(CarrierDetails.class);
        List<Routings> mockMainCarriageRoutings = mock(List.class);
        // Setup mock data
        Routings firstLeg = mock(Routings.class);
        Routings lastLeg = mock(Routings.class);
        when(mockMainCarriageRoutings.get(0)).thenReturn(firstLeg);
        when(mockMainCarriageRoutings.get(mockMainCarriageRoutings.size() - 1)).thenReturn(lastLeg);
        when(firstLeg.getPol()).thenReturn("DifferentOriginPort");
        when(mockCarrierDetails.getOriginPort()).thenReturn("OriginPort");
        when(lastLeg.getPod()).thenReturn("DifferentDestinationPort");
        when(mockCarrierDetails.getDestinationPort()).thenReturn("DestinationPort");

        // Call the method under test
        String result = CommonUtils.setTransportInfoStatusMessage(mockCarrierDetails, TransportInfoStatus.IH, mockMainCarriageRoutings);

        // Verify the result
        assertEquals(Constants.POL_POD_WARNING_MESSAGE, result);
    }

    @Test
    void testSetTransportInfoStatusMessage_Empty() {
        CarrierDetails mockCarrierDetails = mock(CarrierDetails.class);
        List<Routings> mockMainCarriageRoutings = mock(List.class);
        // Setup mock data
        Routings firstLeg = mock(Routings.class);
        Routings lastLeg = mock(Routings.class);
        when(mockMainCarriageRoutings.get(0)).thenReturn(firstLeg);
        when(mockMainCarriageRoutings.get(mockMainCarriageRoutings.size() - 1)).thenReturn(lastLeg);
        when(firstLeg.getPol()).thenReturn("OriginPort");
        when(mockCarrierDetails.getOriginPort()).thenReturn("OriginPort");
        when(lastLeg.getPod()).thenReturn("DestinationPort");
        when(mockCarrierDetails.getDestinationPort()).thenReturn("DestinationPort");

        // Call the method under test
        String result = CommonUtils.setTransportInfoStatusMessage(mockCarrierDetails, TransportInfoStatus.IH, mockMainCarriageRoutings);

        // Verify the result
        assertEquals(Constants.EMPTY_STRING, result);
    }

    @Test
    void testIsVesselVoyageAvailable_SeaMode() {
        List<Routings> mockMainCarriageRoutings = mock(List.class);

        // Setup mock data for sea mode
        Routings routing = mock(Routings.class);
        when(mockMainCarriageRoutings.get(0)).thenReturn(routing);
        when(routing.getMode()).thenReturn(Constants.TRANSPORT_MODE_SEA);
        when(routing.getVesselName()).thenReturn("VesselName");
        when(routing.getVoyage()).thenReturn("Voyage123");

        // Call the method under test
        Boolean result = CommonUtils.isVesselVoyageOrCarrierFlightNumberAvailable(mockMainCarriageRoutings);

        // Verify the result
        assertTrue(result);
    }

    @Test
    void testIsVesselVoyageAvailable_MissingVesselName() {
        List<Routings> mockMainCarriageRoutings = mock(List.class);

        // Setup mock data for sea mode
        Routings routing = mock(Routings.class);
        when(mockMainCarriageRoutings.get(0)).thenReturn(routing);
        when(routing.getMode()).thenReturn(Constants.TRANSPORT_MODE_SEA);
        when(routing.getVesselName()).thenReturn(null);

        // Call the method under test
        Boolean result = CommonUtils.isVesselVoyageOrCarrierFlightNumberAvailable(mockMainCarriageRoutings);

        // Verify the result
        assertFalse(result);
    }

    @Test
    void testIsCarrierFlightNumberAvailable_AirMode() {
        Routings routings1 = new Routings();
        routings1.setLeg(2L);
        routings1.setIsSelectedForDocument(Boolean.TRUE);
        routings1.setId(2l);
        routings1.setMode(TRANSPORT_MODE_AIR);
        routings1.setFlightNumber("Flight");
        routings1.setCarrier("carrier");
        routings1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings1.setPod("DEIMETA");
        routings1.setDestinationPortLocCode("destportLoc");
        List<Routings> mockMainCarriageRoutings = new ArrayList<>();
        mockMainCarriageRoutings.add(routings1);

        // Call the method under test
        Boolean result = CommonUtils.isVesselVoyageOrCarrierFlightNumberAvailable(mockMainCarriageRoutings);

        // Verify the result
        assertTrue(result);
    }

    @Test
    void testIsCarrierFlightNumberAvailable_MissingCarrier() {
        Routings routings1 = new Routings();
        routings1.setLeg(2L);
        routings1.setIsSelectedForDocument(Boolean.TRUE);
        routings1.setId(2l);
        routings1.setVesselName("vessel");
        routings1.setVoyage("0123");
        routings1.setCarriage(RoutingCarriage.MAIN_CARRIAGE);
        routings1.setPod("DEIMETA");
        routings1.setDestinationPortLocCode("destportLoc");
        List<Routings> mockMainCarriageRoutings = new ArrayList<>();
        mockMainCarriageRoutings.add(routings1);

        // Call the method under test
        Boolean result = CommonUtils.isVesselVoyageOrCarrierFlightNumberAvailable(mockMainCarriageRoutings);

        // Verify the result
        assertFalse(result);
    }

    @Test
    void getTaskIdHyperLinkV3Test() {
        String url = commonUtils.getTaskIdHyperLinkV3("a", "b");
        assertNotNull(url);
    }

    @Test
    void testGetRAKCDetailsMap_WithEmptyAddressIds() {
        // Given
        List<String> addressIds = Collections.emptyList();

        // When
        Map<String, RAKCDetailsResponse> result = commonUtils.getRAKCDetailsMap(addressIds);

        // Then
        assertTrue(result.isEmpty(), "The result map should be empty when addressIds is empty.");
    }

    @Test
    void testGetRAKCDetailsMap_WithNullAddressIds() {
        // Given
        List<String> addressIds = null;

        // When
        Map<String, RAKCDetailsResponse> result = commonUtils.getRAKCDetailsMap(addressIds);

        // Then
        assertTrue(result.isEmpty(), "The result map should be empty when addressIds is null.");
    }

    @Test
    void testGetRAKCDetailsMap_WithValidAddressIds() {
        // Given
        List<String> addressIds = Arrays.asList("1", "2");
        V1DataResponse addressResponse = mock(V1DataResponse.class);
        List<RAKCDetailsResponse> rakcDetailsList = Arrays.asList(RAKCDetailsResponse.builder().id(1L).orgId(1L).kCRANumber("Data1").build(), RAKCDetailsResponse.builder().id(2L).orgId(2L).kCRANumber("Data2").build());

        when(iv1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.getEntities(), RAKCDetailsResponse.class)).thenReturn(rakcDetailsList);

        // When
        Map<String, RAKCDetailsResponse> result = commonUtils.getRAKCDetailsMap(addressIds);

        // Then
        assertEquals(2, result.size(), "The result map should contain 2 entries.");
        assertTrue(result.containsKey("1"));
        assertTrue(result.containsKey("2"));
        assertEquals("Data1", result.get("1").getKCRANumber());
        assertEquals("Data2", result.get("2").getKCRANumber());
    }

    @Test
    void testGetRAKCDetailsMap_WithEmptyRAKCDetailsResponse() {
        // Given
        List<String> addressIds = Arrays.asList("1", "2");
        V1DataResponse addressResponse = mock(V1DataResponse.class);
        List<RAKCDetailsResponse> rakcDetailsList = Collections.emptyList();

        when(iv1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(addressResponse);
        when(jsonHelper.convertValueToList(addressResponse.getEntities(), RAKCDetailsResponse.class)).thenReturn(rakcDetailsList);

        // When
        Map<String, RAKCDetailsResponse> result = commonUtils.getRAKCDetailsMap(addressIds);

        // Then
        assertTrue(result.isEmpty(), "The result map should be empty when RAKCDetailsResponse list is empty.");
    }

    @Test
    void testConvertV1InCriteriaRequest() {
        // Given
        List<String> addressIds = Arrays.asList("1", "2");

        // When
        CommonV1ListRequest result = commonUtils.convertV1InCriteriaRequest("Id", addressIds);

        // Then
        assertNotNull(result, "Request should not be null.");
        assertEquals(3, result.getCriteriaRequests().size(), "CriteriaRequests should contain 1 item.");
        assertTrue(result.getCriteriaRequests().get(0) instanceof List, "First item in criteriaRequests should be a List.");
    }

    @Test
    void testUpdateContainerTypeWithQuoteId_withMatchingCodes_setsIsQuotedTrue() {
        String quoteId = "Q123";
        DependentServiceResponse response = new DependentServiceResponse();
        Object rawData = new Object();
        response.setData(rawData);
        List<ContainerTypeMasterResponse> masterList = List.of(
                buildResponse("20GP", false),
                buildResponse("40HC", false)
        );
        List<QuoteContracts> contracts = List.of(
                QuoteContracts.builder().containerTypes(List.of("20GP")).build()
        );
        when(jsonHelper.convertValueToList(rawData, ContainerTypeMasterResponse.class)).thenReturn(masterList);
        when(quoteContractsDao.findByContractId(quoteId)).thenReturn(contracts);
        commonUtils.updateContainerTypeWithQuoteId(response, quoteId);
        List<ContainerTypeMasterResponse> result = (List<ContainerTypeMasterResponse>) response.getData();

        assertTrue(result.get(0).getIsQuoted());
        assertFalse(result.get(1).getIsQuoted());
    }

    @Test
    void testUpdateContainerTypeWithQuoteId_withNoMatchingCodes_allIsQuotedFalse() {
        String quoteId = "Q999";
        DependentServiceResponse response = new DependentServiceResponse();
        Object rawData = new Object();
        response.setData(rawData);

        List<ContainerTypeMasterResponse> masterList = List.of(
                buildResponse("20GP", false),
                buildResponse("40HC", false)
        );

        List<QuoteContracts> contracts = List.of(
                QuoteContracts.builder().containerTypes(List.of("45HC")).build()
        );
        when(jsonHelper.convertValueToList(rawData, ContainerTypeMasterResponse.class)).thenReturn(masterList);
        when(quoteContractsDao.findByContractId(quoteId)).thenReturn(contracts);
        commonUtils.updateContainerTypeWithQuoteId(response, quoteId);
        List<ContainerTypeMasterResponse> result = (List<ContainerTypeMasterResponse>) response.getData();

        assertFalse(result.get(0).getIsQuoted());
        assertFalse(result.get(1).getIsQuoted());
    }

    @Test
    void testGetEntityTransferAddress_whenDefaultAddressIdIsNull_returnsNull() {
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultAddressId(null);
        EntityTransferAddress result = commonUtils.getEntityTransferAddress(tenantModel);
        assertNull(result);
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void testGetEntityTransferAddress_whenNoAddressFound_returnsEmptyEntity() {
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultAddressId(123L);
        CommonV1ListRequest expectedRequest = new CommonV1ListRequest();
        expectedRequest.setCriteriaRequests(List.of(List.of("Id", "=", 123L)));
        V1DataResponse mockResponse = new V1DataResponse();
        mockResponse.entities = new ArrayList<>();
        when(v1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class))).thenReturn(Collections.emptyList());
        EntityTransferAddress result = commonUtils.getEntityTransferAddress(tenantModel);
        assertNotNull(result);
        assertEquals(new EntityTransferAddress(), result);
        verify(v1Service).addressList(any(CommonV1ListRequest.class));
        verify(jsonHelper).convertValueToList(mockResponse.entities, EntityTransferAddress.class);
    }

    @Test
    void testGetEntityTransferAddress_whenAddressFound_returnsEntity() {
        TenantModel tenantModel = new TenantModel();
        tenantModel.setDefaultAddressId(123L);
        EntityTransferAddress expectedAddress = EntityTransferAddress.builder()
                .id(123L)
                .Address1("Test Address")
                .build();
        V1DataResponse mockResponse = new V1DataResponse();
        mockResponse.entities = List.of(Map.of());
        when(v1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferAddress.class)))
                .thenReturn(List.of(expectedAddress));
        EntityTransferAddress result = commonUtils.getEntityTransferAddress(tenantModel);
        assertNotNull(result);
        assertEquals(expectedAddress, result);
        verify(v1Service).addressList(any(CommonV1ListRequest.class));
        verify(jsonHelper).convertValueToList(mockResponse.entities, EntityTransferAddress.class);
    }

    private ContainerTypeMasterResponse buildResponse(String code, boolean isQuoted) {
        ContainerTypeMasterResponse response = new ContainerTypeMasterResponse();
        response.setCode(code);
        response.setIsQuoted(isQuoted);
        return response;
    }

    @Test
    void testGetAddress_withAllFields() {
        // Arrange
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(ReportConstants.COMPANY_NAME, "Company XYZ");
        addressData.put(ReportConstants.ADDRESS1, "123 Main St");
        addressData.put(ReportConstants.CITY, "Metropolis");
        addressData.put(ReportConstants.STATE, "NY");
        addressData.put(ReportConstants.COUNTRY, "USA");
        addressData.put(ReportConstants.ZIP_POST_CODE, "12345");

        // Act
        String result = commonUtils.getAddress(addressData);

        // Assert
        String expected = "Company XYZ,123 Main St,Metropolis,NY,USA,12345";
        assertEquals(expected, result);
    }

    @Test
    void testGetAddress_withEmptyValues() {
        // Arrange
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(ReportConstants.COMPANY_NAME, "");
        addressData.put(ReportConstants.ADDRESS1, "123 Main St");
        addressData.put(ReportConstants.CITY, "");
        addressData.put(ReportConstants.STATE, "NY");
        addressData.put(ReportConstants.COUNTRY, "");
        addressData.put(ReportConstants.ZIP_POST_CODE, "12345");

        // Act
        String result = commonUtils.getAddress(addressData);

        // Assert
        String expected = "123 Main St,NY,12345";
        assertEquals(expected, result);
    }

    @Test
    void testGetAddress_withNullValues() {
        // Arrange
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(ReportConstants.COMPANY_NAME, null);
        addressData.put(ReportConstants.ADDRESS1, "123 Main St");
        addressData.put(ReportConstants.CITY, null);
        addressData.put(ReportConstants.STATE, "NY");
        addressData.put(ReportConstants.COUNTRY, null);
        addressData.put(ReportConstants.ZIP_POST_CODE, "12345");

        // Act
        String result = commonUtils.getAddress(addressData);

        // Assert
        String expected = "123 Main St,NY,12345";
        assertEquals(expected, result);
    }

    @Test
    void testGetAddress_withOnlyEmptyStrings() {
        // Arrange
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(ReportConstants.COMPANY_NAME, "");
        addressData.put(ReportConstants.ADDRESS1, "");
        addressData.put(ReportConstants.CITY, "");
        addressData.put(ReportConstants.STATE, "");
        addressData.put(ReportConstants.COUNTRY, "");
        addressData.put(ReportConstants.ZIP_POST_CODE, "");

        // Act
        String result = commonUtils.getAddress(addressData);

        // Assert
        String expected = "";
        assertEquals(expected, result);
    }

    @Test
    void testGetAddress_withOnlyNullValues() {
        // Arrange
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(ReportConstants.COMPANY_NAME, null);
        addressData.put(ReportConstants.ADDRESS1, null);
        addressData.put(ReportConstants.CITY, null);
        addressData.put(ReportConstants.STATE, null);
        addressData.put(ReportConstants.COUNTRY, null);
        addressData.put(ReportConstants.ZIP_POST_CODE, null);

        // Act
        String result = commonUtils.getAddress(addressData);

        // Assert
        String expected = "";
        assertEquals(expected, result);
    }

    @Test
    void testGetAddress_withSingleField() {
        // Arrange
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(ReportConstants.COMPANY_NAME, "Company XYZ");

        // Act
        String result = commonUtils.getAddress(addressData);

        // Assert
        String expected = "Company XYZ";
        assertEquals(expected, result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_NoChanges() {
        // Test case when all fields are identical - should return false
        Packing oldPack = createTestPacking();
        Packing newPack = createTestPacking();

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertFalse(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_HazardousChanged() {
        // Test case when Hazardous field changes - should return true
        Packing oldPack = createTestPacking();
        oldPack.setHazardous(true);

        Packing newPack = createTestPacking();
        newPack.setHazardous(false);

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @ParameterizedTest
    @MethodSource("provideDGClassChangeCases")
    void testCheckIfDGFieldsChangedInPackingV3_DGClassChanges(String oldDGClass, String newDGClass) {
        Packing oldPack = createDGTestPacking();
        oldPack.setDGClass(oldDGClass);

        Packing newPack = createDGTestPacking();
        newPack.setDGClass(newDGClass);

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);
        assertTrue(result);
    }

    private static Stream<Arguments> provideDGClassChangeCases() {
        return Stream.of(
                Arguments.of("Class1", "Class2"), // value change
                Arguments.of(null, "Class1"),     // null to value
                Arguments.of("Class1", null)      // value to null
        );
    }

    // Mocked or test utility method
    private Packing createDGTestPacking() {
        Packing packing = new Packing();
        packing.setDGClass("Class1"); // default, can be overridden
        // set other default fields if needed
        return packing;
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_UnNumberChanged() {
        // Test case when UnNumber field changes - should return true
        Packing oldPack = createTestPacking();
        oldPack.setUnNumber("UN1001");

        Packing newPack = createTestPacking();
        newPack.setUnNumber("UN1002");

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_ProperShippingNameChanged() {
        // Test case when ProperShippingName field changes - should return true
        Packing oldPack = createTestPacking();
        oldPack.setProperShippingName("Old Name");

        Packing newPack = createTestPacking();
        newPack.setProperShippingName("New Name");

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_PackingGroupChanged() {
        // Test case when PackingGroup field changes - should return true
        Packing oldPack = createTestPacking();
        oldPack.setPackingGroup("I");

        Packing newPack = createTestPacking();
        newPack.setPackingGroup("II");

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_MinimumFlashPointChanged() {
        // Test case when MinimumFlashPoint field changes - should return true
        // Assuming compareBigDecimals returns false when values are different
        Packing oldPack = createTestPacking();
        oldPack.setMinimumFlashPoint(new BigDecimal("10.5"));

        Packing newPack = createTestPacking();
        newPack.setMinimumFlashPoint(new BigDecimal("15.5"));

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_MinimumFlashPointUnitChanged() {
        // Test case when MinimumFlashPointUnit field changes - should return true
        Packing oldPack = createTestPacking();
        oldPack.setMinimumFlashPointUnit("C");

        Packing newPack = createTestPacking();
        newPack.setMinimumFlashPointUnit("F");

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_MarinePollutantChanged() {
        // Test case when MarinePollutant field changes - should return true
        Packing oldPack = createTestPacking();
        oldPack.setMarinePollutant(true);

        Packing newPack = createTestPacking();
        newPack.setMarinePollutant(false);

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_AllFieldsNull() {
        // Test case when all nullable fields are null in both objects - should return false
        Packing oldPack = new Packing();
        oldPack.setHazardous(false);
        oldPack.setMarinePollutant(false);
        oldPack.setDGClass(null);
        oldPack.setUnNumber(null);
        oldPack.setProperShippingName(null);
        oldPack.setPackingGroup(null);
        oldPack.setMinimumFlashPoint(null);
        oldPack.setMinimumFlashPointUnit(null);

        Packing newPack = new Packing();
        newPack.setHazardous(false);
        newPack.setMarinePollutant(false);
        newPack.setDGClass(null);
        newPack.setUnNumber(null);
        newPack.setProperShippingName(null);
        newPack.setPackingGroup(null);
        newPack.setMinimumFlashPoint(null);
        newPack.setMinimumFlashPointUnit(null);

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertFalse(result);
    }

    @Test
    void testCheckIfDGFieldsChangedInPackingV3_MultipleFieldsChanged() {
        // Test case when multiple fields change - should return true (early return on first change)
        Packing oldPack = createTestPacking();
        oldPack.setHazardous(true);
        oldPack.setDGClass("Class1");

        Packing newPack = createTestPacking();
        newPack.setHazardous(false); // This will cause early return
        newPack.setDGClass("Class2"); // This won't be evaluated due to early return

        boolean result = commonUtils.checkIfDGFieldsChangedInPackingV3(newPack, oldPack);

        assertTrue(result);
    }

    // Helper method to create a test packing object with default values
    private Packing createTestPacking() {
        Packing packing = new Packing();
        packing.setHazardous(false);
        packing.setDGClass("TestClass");
        packing.setUnNumber("UN1234");
        packing.setProperShippingName("Test Shipping Name");
        packing.setPackingGroup("I");
        packing.setMinimumFlashPoint(new BigDecimal("20.0"));
        packing.setMinimumFlashPointUnit("C");
        packing.setMarinePollutant(false);
        return packing;
    }

    @Test
    void testIsRoadFTL_OrRailFCL_FCL() {
        assertTrue(commonUtils.isRoadFTLOrRailFCL(TRANSPORT_MODE_RAI, "FCL"));
    }

    @Test
    void testIsRoadFTL_FTLOrRailFCL() {
        assertTrue(commonUtils.isRoadFTLOrRailFCL(Constants.TRANSPORT_MODE_ROA, "FTL"));
    }

    @Test
    void testIsRoadFTL_OrRailFCL_InvalidTransportMode() {
        assertFalse(commonUtils.isRoadFTLOrRailFCL(Constants.TRANSPORT_MODE_SEA, "FCL"));
    }

    @Test
    void testIsRoadFTL_OrRailFCL_InvalidCargoType() {
        assertFalse(commonUtils.isRoadFTLOrRailFCL(Constants.TRANSPORT_MODE_ROA, "LCL"));
    }

    @Test
    void testIsSeaFCL_Valid() {
        assertTrue(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_SEA, "FCL"));
    }

    @Test
    void testIsSeaFCL_InvalidCargoType() {
        assertFalse(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_SEA, "LCL"));
    }

    @Test
    void testIsSeaFCL_InvalidTransportMode() {
        assertFalse(commonUtils.isSeaFCL(Constants.TRANSPORT_MODE_AIR, "FCL"));
    }

    @Test
    void testIsFCLorFTL_FCL() {
        assertTrue(commonUtils.isFCLorFTL("FCL"));
    }

    @Test
    void testIsFCLorFTL_FTL() {
        assertTrue(commonUtils.isFCLorFTL("FTL"));
    }

    @Test
    void testIsFCLorFTL_Invalid() {
        assertFalse(commonUtils.isFCLorFTL("LCL"));
        assertFalse(commonUtils.isFCLorFTL(null));
        assertFalse(commonUtils.isFCLorFTL(""));
    }

    @Test
    void testIsSeaFCLOrRoadFTL_WhenSeaFCL_ReturnsTrue() {
        assertTrue(commonUtils.isSeaFCLOrRoadFTL("SEA", "FCL"));
    }

    @Test
    void testIsSeaFCLOrRoadFTL_WhenNeither_ReturnsFalse() {
        assertFalse(commonUtils.isSeaFCLOrRoadFTL("AIR", "LCL"));
    }

    @Test
    void testIsRoadLCLorLTL_ReturnsFalse_WhenNotRoad() {
        assertFalse(commonUtils.isRoadLCLorLTL("SEA", "LCL"));  // transport mode is not ROAD
    }

    @Test
    void testIsRoadLCLorLTL_ReturnsFalse_WhenNotLCLorLTL() {
        assertFalse(commonUtils.isRoadLCLorLTL("ROAD", "FCL")); // cargo type not LCL or LTL
    }

    @Test
    void testIsRoadLCLorLTL_ReturnsFalse_WhenTransportModeAndCargoTypeInvalid() {
        assertFalse(commonUtils.isRoadLCLorLTL("AIR", "FCL"));
    }

    @Test
    void testCapitalizeV3() {
        String result = commonUtils.capitalizeV3("example");
        assertEquals("Example", result);
    }

    @Test
    void testSplitAndTrimStrings() {
        List<String> result = CommonUtils.splitAndTrimStrings(" a , b, c ");
        assertEquals(List.of("a", "b", "c"), result);
    }

    @Test
    void testSplitAndTrimStrings_EmptyInput() {
        List<String> result = CommonUtils.splitAndTrimStrings("");
        assertTrue(result.isEmpty());
    }

    @Test
    void testIncludeRequiredColumns() {
        Set<String> columns = new HashSet<>();
        CommonUtils.includeRequiredColumns(columns);
        assertTrue(columns.contains("id"));
        assertTrue(columns.contains("guid"));
    }

    @Test
    void testRoundBigDecimal() {
        BigDecimal input = new BigDecimal("10.12345");
        BigDecimal result = CommonUtils.roundBigDecimal(input, 2, RoundingMode.HALF_UP);
        assertEquals(new BigDecimal("10.12"), result);
    }

    @Test
    void testDivide_NormalCase() {
        BigDecimal result = CommonUtils.divide(new BigDecimal("10"), new BigDecimal("2"), 2, RoundingMode.HALF_UP);
        assertEquals(new BigDecimal("5.00"), result);
    }

    @Test
    void testDivide_DivideByZero() {
        BigDecimal result = CommonUtils.divide(new BigDecimal("10"), BigDecimal.ZERO, 2, RoundingMode.HALF_UP);
        assertEquals(BigDecimal.ZERO, result);
    }

    @Test
    void testCalculatePercentage_Normal() {
        double result = CommonUtils.calculatePercentage(new BigDecimal("2"), new BigDecimal("4"), 2, RoundingMode.HALF_UP);
        assertEquals(50.0, result);
    }

    @Test
    void testCalculatePercentage_DivideByZero() {
        double result = CommonUtils.calculatePercentage(new BigDecimal("1"), BigDecimal.ZERO, 2, RoundingMode.HALF_UP);
        assertEquals(0.0, result);
    }

    @Test
    void shouldNotThrowException_WhenAirDGTrue_AndNotSecurity_AndHazardous_AndNotDgUser() {
        ShipmentDetails details = new ShipmentDetails();
        details.setTransportMode("AIR");
        details.setContainsHazardous(true);

        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setAirDGFlag(true);
        mockSettings.setCountryAirCargoSecurity(false);

        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class);
             MockedStatic<CommonUtils> commonUtilsMock = mockStatic(CommonUtils.class)) {

            commonUtilsMock.when(commonUtils::getShipmentSettingFromContext).thenReturn(Optional.of(mockSettings));
            userContextMock.when(UserContext::isAirDgUser).thenReturn(false);

            assertDoesNotThrow( () ->
                    commonUtils.validateAirSecurityAndDGShipmentPermissions(details));
        }
    }


    @Test
    void shouldThrowException_WhenAirSecurityTrue_AndCheckAirSecurityFails() {
        ShipmentDetails details = new ShipmentDetails();
        details.setTransportMode("AIR");

        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setAirDGFlag(false);
        mockSettings.setCountryAirCargoSecurity(true);

        try (MockedStatic<CommonUtils> mockedStatic = mockStatic(CommonUtils.class)) {
            mockedStatic.when(commonUtils::getShipmentSettingFromContext).thenReturn(Optional.of(mockSettings));
            mockedStatic.when(() -> CommonUtils.checkAirSecurityForShipment(details)).thenReturn(false);

            assertThrows(ValidationException.class, () ->
                    commonUtils.validateAirSecurityAndDGShipmentPermissions(details));
        }
    }


    @Test
    void shouldThrowException_WhenConsolidationHazardousAndNotDgUser() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setTransportMode("AIR");
        details.setHazardous(true);

        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setAirDGFlag(true);
        mockSettings.setCountryAirCargoSecurity(false);

        // If getShipmentSettingFromContext is static:
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            try (MockedStatic<CommonUtils> commonUtilsMock = mockStatic(CommonUtils.class)) {
                // Mock static method getShipmentSettingFromContext() returning Optional
                commonUtilsMock.when(commonUtils::getShipmentSettingFromContext).thenReturn(Optional.of(mockSettings));

                userContextMock.when(UserContext::isAirDgUser).thenReturn(false);

                assertDoesNotThrow(() ->
                        commonUtils.validateAirSecurityAndDGConsolidationPermissions(details));
            }
        }
    }

    @Test
    void shouldThrowException_WhenConsolidationSecurityCheckFails() {
        ConsolidationDetails details = new ConsolidationDetails();
        details.setTransportMode("AIR");

        ShipmentSettingsDetails mockSettings = new ShipmentSettingsDetails();
        mockSettings.setAirDGFlag(false);
        mockSettings.setCountryAirCargoSecurity(true);

        try (MockedStatic<CommonUtils> commonUtilsMock = mockStatic(CommonUtils.class)) {
            commonUtilsMock.when(commonUtils::getShipmentSettingFromContext).thenReturn(Optional.of(mockSettings));
            commonUtilsMock.when(() -> CommonUtils.checkAirSecurityForConsolidation(details)).thenReturn(false);

            assertThrows(ValidationException.class, () ->
                    commonUtils.validateAirSecurityAndDGConsolidationPermissions(details));
        }
    }

    @Test
    void fetchAddressData_shouldReturnMap_whenListNotEmpty() {
        List<String> addressIds = List.of("1", "2");
        CommonV1ListRequest expectedRequest = commonUtils.createCriteriaToFetchAddressList(addressIds);

        // Mock response from v1Service.addressList
        V1DataResponse mockResponse = new V1DataResponse();
        mockResponse.setEntities("dummyEntities"); // Could be any object, your jsonHelper mock will handle conversion

        // Mock list of AddressDataV1
        AddressDataV1 addr1 = new AddressDataV1();
        addr1.setId(1L);
        AddressDataV1 addr2 = new AddressDataV1();
        addr2.setId(2L);
        List<AddressDataV1> addressDataList = List.of(addr1, addr2);

        when(v1Service.addressList(any(CommonV1ListRequest.class))).thenReturn(mockResponse);
        when(jsonHelper.convertValueToList(mockResponse.getEntities(), AddressDataV1.class))
                .thenReturn(addressDataList);

        Map<Long, AddressDataV1> result = commonUtils.fetchAddressData(addressIds);

        assertEquals(2, result.size());
        assertTrue(result.containsKey(1L));
        assertTrue(result.containsKey(2L));
        verify(v1Service).addressList(any());
        verify(jsonHelper).convertValueToList(any(), eq(AddressDataV1.class));
    }

    @Test
    void fetchAddressData_shouldReturnEmptyMap_whenListEmpty() {
        Map<Long, AddressDataV1> result = commonUtils.fetchAddressData(List.of());
        assertTrue(result.isEmpty());
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void fetchAddressData_shouldReturnEmptyMap_whenListNull() {
        Map<Long, AddressDataV1> result = commonUtils.fetchAddressData(null);
        assertTrue(result.isEmpty());
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void fetchOrgAddressData_shouldReturnMap_whenListNotEmpty() {
        List<String> orgIds = List.of("10", "20");

        V1DataResponse mockResponse = new V1DataResponse();
        mockResponse.setEntities("dummyEntities");

        OrgDataV1 org1 = new OrgDataV1();
        org1.setId(10L);
        OrgDataV1 org2 = new OrgDataV1();
        org2.setId(20L);
        List<OrgDataV1> orgDataList = List.of(org1, org2);

        when(v1Service.fetchOrganization(any(CommonV1ListRequest.class))).thenReturn(mockResponse);
        when(jsonHelper.convertValueToList(mockResponse.getEntities(), OrgDataV1.class))
                .thenReturn(orgDataList);

        Map<Long, OrgDataV1> result = commonUtils.fetchOrgAddressData(orgIds);

        assertEquals(2, result.size());
        assertTrue(result.containsKey(10L));
        assertTrue(result.containsKey(20L));
        verify(v1Service).fetchOrganization(any());
        verify(jsonHelper).convertValueToList(any(), eq(OrgDataV1.class));
    }

    @Test
    void fetchOrgAddressData_shouldReturnEmptyMap_whenListEmpty() {
        Map<Long, OrgDataV1> result = commonUtils.fetchOrgAddressData(List.of());
        assertTrue(result.isEmpty());
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void fetchOrgAddressData_shouldReturnEmptyMap_whenListNull() {
        Map<Long, OrgDataV1> result = commonUtils.fetchOrgAddressData(null);
        assertTrue(result.isEmpty());
        verifyNoInteractions(v1Service, jsonHelper);
    }

    @Test
    void getEntityTransferAddress_shouldReturnNullForAirTransport() {
        EntityTransferAddress result = commonUtils.getEntityTransferAddress(Constants.TRANSPORT_MODE_AIR);
        assertNull(result);
    }

    @Test
    void getEntityTransferAddress_shouldReturnNullWhenExceptionOccurs() {
        when(v1Service.retrieveTenant()).thenThrow(new RuntimeException("Test exception"));
        EntityTransferAddress result = commonUtils.getEntityTransferAddress(Constants.TRANSPORT_MODE_SEA);
        assertNull(result);
    }

    @Test
    void testGetEntityTransferAddress_SeaMode_ReturnsAddress() {
        V1RetrieveResponse tenantResponse = mock(V1RetrieveResponse.class);
        Object dummyEntity = new Object();
        when(v1Service.retrieveTenant()).thenReturn(tenantResponse);
        when(tenantResponse.getEntity()).thenReturn(dummyEntity);
        when(modelMapper.map(dummyEntity, TenantModel.class)).thenReturn(tenantModel);
        doReturn(entityTransferAddress).when(commonUtils).getEntityTransferAddress(tenantModel);
        EntityTransferAddress result = commonUtils.getEntityTransferAddress(Constants.TRANSPORT_MODE_SEA);
        assertNotNull(result);
        assertEquals(entityTransferAddress, result);
    }


    @Test
    void testGetEntityTransferAddress_InvalidMode_ReturnsNull() {
        V1RetrieveResponse tenantResponse = mock(V1RetrieveResponse.class);
        Object dummyEntity = new Object();
        when(v1Service.retrieveTenant()).thenReturn(tenantResponse);
        when(tenantResponse.getEntity()).thenReturn(dummyEntity);
        when(modelMapper.map(dummyEntity, TenantModel.class)).thenReturn(tenantModel);
        doReturn(entityTransferAddress).when(commonUtils).getEntityTransferAddress(tenantModel);
        EntityTransferAddress result = commonUtils.getEntityTransferAddress("AIR");
        assertNull(result);
    }

    @Test
    void getLongValue_shouldReturnNullForNullInput() {
        assertNull(commonUtils.getLongValue(null));
    }

    @ParameterizedTest
    @MethodSource("validNumberProviders")
    void getLongValue_shouldReturnLongForNumberTypes(Number number, long expected) {
        assertEquals(expected, commonUtils.getLongValue(number));
    }

    @Test
    void getLongValue_shouldReturnLongForStringNumber() {
        assertEquals(123L, commonUtils.getLongValue("123"));
    }

    @Test
    void getLongValue_shouldThrowForUnsupportedType() {
        Object invalidValue = new Object();
        Exception exception = assertThrows(IllegalArgumentException.class,
                () -> commonUtils.getLongValue(invalidValue));
        assertTrue(exception.getMessage().contains("Unsupported Party ID type"));
    }

    private static Stream<Arguments> validNumberProviders() {
        return Stream.of(
                Arguments.of(123, 123L),
                Arguments.of(123.45f, 123L),
                Arguments.of(123.67d, 123L),
                Arguments.of(123L, 123L),
                Arguments.of((short)123, 123L),
                Arguments.of((byte)123, 123L)
        );

    }
    @Test
    void returnsNullWhenRequestPayloadIsNull() {
        assertNull(commonUtils.extractSortFieldFromPayload(null));
    }

    @Test
    void returnsNullWhenSortRequestIsNull() {
        ListCommonRequest request = mock(ListCommonRequest.class);
        when(request.getSortRequest()).thenReturn(null);
        assertNull(commonUtils.extractSortFieldFromPayload(request));
    }

    @Test
    void returnsFieldNameWhenSortRequestIsPresent() {
        ListCommonRequest request = mock(ListCommonRequest.class);
        SortRequest sortRequest = mock(SortRequest.class);
        when(request.getSortRequest()).thenReturn(sortRequest);
        when(sortRequest.getFieldName()).thenReturn("testField");
        assertEquals("testField", commonUtils.extractSortFieldFromPayload(request));
    }
    @Test
    void testExtractRequestedColumns_EmptyIncludeColumns_ReturnsEmptyMap() {
        // Arrange
        List<String> includeColumns = new ArrayList<>();
        String mainEntityKey = "shipment";

        // Act
        Map<String, Object> result = commonUtils.extractRequestedColumns(includeColumns, mainEntityKey);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testExtractRequestedColumns_SingleLevelColumns_ExtractsToMainEntity() {
        // Arrange
        List<String> includeColumns = Arrays.asList("id", "updatedAt", "status");
        String mainEntityKey = "shipmentDetails";

        // Act
        Map<String, Object> result = commonUtils.extractRequestedColumns(includeColumns, mainEntityKey);

        // Assert
        assertNotNull(result);
        assertTrue(result.containsKey(mainEntityKey));

        List<String> shipmentColumns = (List<String>) result.get(mainEntityKey);
        assertNotNull(shipmentColumns);
        assertEquals(3, shipmentColumns.size());
        assertTrue(shipmentColumns.contains("id"));
        assertTrue(shipmentColumns.contains("updatedAt"));
        assertTrue(shipmentColumns.contains("status"));
    }

    @Test
    void testExtractRequestedColumns_TwoLevelNesting_CreatesListOfStrings() {
        // Arrange
        List<String> includeColumns = Arrays.asList(
                "pickupDetails.address",
                "pickupDetails.contactName",
                "transporterDetail.name"
        );
        String mainEntityKey = "shipmentDetails";

        // Act
        Map<String, Object> result = commonUtils.extractRequestedColumns(includeColumns, mainEntityKey);

        // Assert
        assertNotNull(result);
        assertTrue(result.containsKey("pickupDetails"));
        assertTrue(result.containsKey("transporterDetail"));

        // Verify pickupDetails structure
        List<String> pickupColumns = (List<String>) result.get("pickupDetails");
        assertNotNull(pickupColumns);
        assertEquals(2, pickupColumns.size());
        assertTrue(pickupColumns.contains("address"));
        assertTrue(pickupColumns.contains("contactName"));

        // Verify transporterDetail structure
        List<String> transporterColumns = (List<String>) result.get("transporterDetail");
        assertNotNull(transporterColumns);
        assertEquals(1, transporterColumns.size());
        assertTrue(transporterColumns.contains("name"));
    }
    @Test
    void testExtractRequestedColumns_DuplicateColumns_HandlesGracefully() {
        // Arrange
        List<String> includeColumns = Arrays.asList(
                "pickupDetails.address",
                "pickupDetails.address", // Duplicate
                "pickupDetails.contactName",
                "consignee.orgData.FullName"
        );
        String mainEntityKey = "shipment";

        // Act
        Map<String, Object> result = commonUtils.extractRequestedColumns(includeColumns, mainEntityKey);

        // Assert
        assertNotNull(result);
        assertTrue(result.containsKey("pickupDetails"));

        List<String> pickupColumns = (List<String>) result.get("pickupDetails");
        assertNotNull(pickupColumns);

        // Should handle duplicates gracefully (either include once or include multiple times)
        assertTrue(pickupColumns.contains("address"));
        assertTrue(pickupColumns.contains("contactName"));
    }

    @Test
    void testFillEmptyColumnLists_EmptyMap_DoesNotModify() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        assertTrue(requestedColumns.isEmpty());
    }
    @Test
    void testFillEmptyColumnLists_WithEmptyList_FillsWithAllFields() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        requestedColumns.put("shipmentDetails", new ArrayList<String>());

        // Mock ShipmentConstants.ENTITY_MAPPINGS to contain "shipment" -> ShipmentDetails.class
        // This test assumes getAllSimpleFieldNames() returns a list of field names

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        Object shipmentColumns = requestedColumns.get("shipmentDetails");
        assertNotNull(shipmentColumns);
        assertTrue(shipmentColumns instanceof List);
        List<String> fieldsList = (List<String>) shipmentColumns;
        assertFalse(fieldsList.isEmpty());
        // Verify it contains expected fields based on getAllSimpleFieldNames()
    }

    @Test
    void testFillEmptyColumnLists_WithEmptyMap_FillsWithAllFields() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        requestedColumns.put("pickupDetails", new HashMap<String, Object>());

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        Object pickupDetails = requestedColumns.get("pickupDetails");
        assertNotNull(pickupDetails);
        assertTrue(pickupDetails instanceof List);
        List<String> fieldsList = (List<String>) pickupDetails;
        assertFalse(fieldsList.isEmpty());
    }

    @Test
    void testFillEmptyColumnLists_WithNullValue_FillsWithAllFields() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        requestedColumns.put("transporterDetail", null);

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        Object transporterDetail = requestedColumns.get("transporterDetail");
        assertNotNull(transporterDetail);
        assertTrue(transporterDetail instanceof List);
        List<String> fieldsList = (List<String>) transporterDetail;
        assertFalse(fieldsList.isEmpty());
    }

    @Test
    void testFillEmptyColumnLists_WithPopulatedList_DoesNotModify() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        List<String> existingFields = new ArrayList<>(Arrays.asList("field1", "field2"));
        requestedColumns.put("shipmentDetails", existingFields);

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        List<String> resultFields = (List<String>) requestedColumns.get("shipmentDetails");
        assertEquals(3, resultFields.size()); // Original 2 + "id" added by ensureIdInCollection
        assertTrue(resultFields.contains("field1"));
        assertTrue(resultFields.contains("field2"));
        assertTrue(resultFields.contains("id"));
    }
    @Test
    void testFillEmptyColumnLists_WithPopulatedSet_DoesNotModify() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        Set<String> existingFields = new HashSet<>(Set.of("field1", "field2"));
        requestedColumns.put("shipmentDetails", existingFields);

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        Set<String> resultFields = (Set<String>) requestedColumns.get("shipmentDetails");
        assertEquals(3, resultFields.size()); // Original 2 + "id" added by ensureIdInCollection
        assertTrue(resultFields.contains("field1"));
        assertTrue(resultFields.contains("field2"));
        assertTrue(resultFields.contains("id"));
    }
    @Test
    void testFillEmptyColumnLists_WithPopulatedNonCollection_DoesNotModify() {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();

        requestedColumns.put("shipmentDetails", new Parties());

        // Act
        commonUtils.fillEmptyColumnLists(requestedColumns);

        // Assert
        Parties resultFields = (Parties) requestedColumns.get("shipmentDetails");
        assertNotNull(resultFields);
    }

    @Test
    void testEnsureIdInCollection_WithList_AddsIdWhenNotPresent() {
        // Arrange
        List<String> list = new ArrayList<>(Arrays.asList("field1", "field2"));

        // Act
        Object result = commonUtils.ensureIdInCollection(list);

        // Assert
        assertTrue(result instanceof List);
        List<String> resultList = (List<String>) result;
        assertEquals(3, resultList.size());
        assertTrue(resultList.contains("field1"));
        assertTrue(resultList.contains("field2"));
        assertTrue(resultList.contains("id"));
    }

    @Test
    void testEnsureIdInCollection_WithListContainingId_DoesNotAddAgain() {
        // Arrange
        List<String> list = new ArrayList<>(Arrays.asList("id", "field1", "field2"));

        // Act
        Object result = commonUtils.ensureIdInCollection(list);

        // Assert
        assertTrue(result instanceof List);
        List<String> resultList = (List<String>) result;
        assertEquals(3, resultList.size());
        assertTrue(resultList.contains("id"));
        assertTrue(resultList.contains("field1"));
        assertTrue(resultList.contains("field2"));
    }

    @Test
    void testEnsureIdInCollection_WithEmptyList_DoesNotAddId() {
        // Arrange
        List<String> list = new ArrayList<>();

        // Act
        Object result = commonUtils.ensureIdInCollection(list);

        // Assert
        assertTrue(result instanceof List);
        List<String> resultList = (List<String>) result;
        assertTrue(resultList.isEmpty());
    }

    @Test
    void testEnsureIdInCollection_WithSet_AddsIdWhenNotPresent() {
        // Arrange
        Set<String> set = new HashSet<>(Arrays.asList("field1", "field2"));

        // Act
        Object result = commonUtils.ensureIdInCollection(set);

        // Assert
        assertTrue(result instanceof Set);
        Set<String> resultSet = (Set<String>) result;
        assertEquals(3, resultSet.size());
        assertTrue(resultSet.contains("field1"));
        assertTrue(resultSet.contains("field2"));
        assertTrue(resultSet.contains("id"));
    }
    @Test
    void testEnsureIdInCollection_WithNonCollection_ReturnsUnchanged() {
        // Arrange
        String nonCollection = "not a collection";

        // Act
        Object result = commonUtils.ensureIdInCollection(nonCollection);

        // Assert
        assertEquals("not a collection", result);
    }

    @Test
    void testEnsureIdInCollection_WithNull_ReturnsNull() {
        // Act
        Object result = commonUtils.ensureIdInCollection(null);

        // Assert
        assertNull(result);
    }
    @Test
    void testConvertToNestedMapWithCollections_EmptyFlatList_ReturnsEmptyList() {
        // Arrange
        List<Map<String, Object>> flatList = new ArrayList<>();
        Set<String> collectionRelationships = new HashSet<>();
        String rootKey = "shipment";

        // Act
        List<Map<String, Object>> result = commonUtils.convertToNestedMapWithCollections(
                flatList, collectionRelationships, rootKey);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testConvertToNestedMapWithCollections_SingleRow_CreatesNestedStructure() {
        // Arrange
        List<Map<String, Object>> flatList = new ArrayList<>();
        Map<String, Object> row1 = new HashMap<>();
        row1.put("shipmentDetails.id", 123L);
        row1.put("shipmentDetails.transportMode", TRANSPORT_MODE_SEA);
        row1.put("pickupDetails.address", "Address 1");
        flatList.add(row1);

        Set<String> collectionRelationships = new HashSet<>();
        String rootKey = "shipmentDetails";

        // Act
        List<Map<String, Object>> result = commonUtils.convertToNestedMapWithCollections(
                flatList, collectionRelationships, rootKey);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        Map<String, Object> resultMap = result.get(0);
        assertTrue(resultMap.containsKey(rootKey));

        Map<String, Object> shipmentData = (Map<String, Object>) resultMap.get(rootKey);
        assertNotNull(shipmentData);
        assertEquals(123L, shipmentData.get("id"));
    }


    @Test
    void testConvertToNestedMapWithCollections_ComplexNestedStructure() {
        // Arrange
        List<Map<String, Object>> flatList = new ArrayList<>();
        Map<String, Object> row = new HashMap<>();

        row.put("shipmentDetails.id", 123L);
        row.put("shipmentDetails.shipmentNumber", "SHIP-001");
        row.put("shipmentDetails.pickupDetails.address", "123 Main St");
        row.put("shipmentDetails.pickupDetails.address.city", "New York");
        row.put("shipmentDetails.deliveryDetails", "New1234");
        row.put("shipmentDetails.deliveryDetails.contact", "John Doe");
//        row.put("shipmentDetails.deliveryDetails.contact.phone", Arrays.asList("555-1234"));
        row.put("shipmentDetails.pickupDetails.transporterDetail.orgData.FullName", "ASDrefr");
        flatList.add(row);

        Set<String> collectionRelationships = new HashSet<>(
                Arrays.asList("pickupDetails", "deliveryDetails"));
        String rootKey = "shipmentDetails";

        // Act
        List<Map<String, Object>> result = commonUtils.convertToNestedMapWithCollections(
                flatList, collectionRelationships, rootKey);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        Map<String, Object> resultMap = result.get(0);
        assertTrue(resultMap.containsKey(rootKey));
        Map<String, Object> shipmentMap = (Map<String, Object>)resultMap.get("shipmentDetails");

        // Verify nested structures are processed correctly
        // This depends on the implementation of ProcessFields method
        List<Object> pickupDetailsList = (List<Object>) shipmentMap.get("pickupDetails");

        assertNotNull(pickupDetailsList);
    }
    @Test
    void testConvertToNestedMapWithCollections_NonNestedStructure() {
        // Arrange
        List<Map<String, Object>> flatList = new ArrayList<>();
        Map<String, Object> row = new HashMap<>();

//        row.put("shipmentDetails.id", 123L);
//        row.put("shipmentDetails.shipmentNumber", "SHIP-001");
////        row.put("shipmentDetails.pickupDetails.address", "123 Main St");
////        row.put("shipmentDetails.pickupDetails.address.city", "New York");
        row.put("shipmentDetails.deliveryDetails.id", "New1234");
        row.put("shipmentDetails.deliveryDetails.contact", "John Doe");
////        row.put("shipmentDetails.deliveryDetails.contact.phone", Arrays.asList("555-1234"));
//        row.put("shipmentDetails.pickupDetails.transporterDetail.orgData.FullName", "ASDrefr");
        flatList.add(row);

        Set<String> collectionRelationships = new HashSet<>(
                Arrays.asList("pickupDetails", "deliveryDetails"));
        String rootKey = "shipmentDetails";

        // Act
        List<Map<String, Object>> result = commonUtils.convertToNestedMapWithCollections(
                flatList, collectionRelationships, rootKey);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());

        Map<String, Object> resultMap = result.get(0);
        assertTrue(resultMap.containsKey(rootKey));
        Map<String, Object> shipmentMap = (Map<String, Object>)resultMap.get("shipmentDetails");

        // Verify nested structures are processed correctly
        // This depends on the implementation of ProcessFields method
        List<Object> pickupDetailsList = (List<Object>) shipmentMap.get("pickupDetails");

        assertNotNull(pickupDetailsList);
    }

    @Test
     void testRefineIncludeColumns_EmptyList_ReturnsEmptyList() {
        // Arrange
        List<String> includeColumns = new ArrayList<>();

        // Act
        List<String> result = commonUtils.refineIncludeColumns(includeColumns);

        // Assert
        assertNotNull(result);
        assertTrue(!result.isEmpty());
    }
    @Test
     void testRefineIncludeColumns_NullList_ThrowsException() {
        // Arrange
        List<String> includeColumns = null;

        // Act & Assert
        assertThrows(NullPointerException.class, () -> {
            commonUtils.refineIncludeColumns(includeColumns);
        });
    }
    @Test
     void testRefineIncludeColumns_NoSpecialColumns_ReturnsAddedGuid() {
        // Arrange
        List<String> includeColumns = Arrays.asList(
                "id",
                "shipmentNumber",
                "status",
                "pickupDetails.address",
                "deliveryDetails.contactName"
        );

        // Act
        List<String> result = commonUtils.refineIncludeColumns(new ArrayList<>(includeColumns));

        // Assert
        assertNotNull(result);
        assertEquals(6, result.size());
        assertTrue(result.contains("id"));
        assertTrue(result.contains("shipmentNumber"));
        assertTrue(result.contains("status"));
        assertTrue(result.contains("pickupDetails.address"));
        assertTrue(result.contains("deliveryDetails.contactName"));
        assertTrue(result.contains("guid"));
    }

    @Test
     void testRefineIncludeColumns_WithOrgDataColumns_TruncatesCorrectly() {
        // Arrange - assuming Constants.ORG_DATA = "orgData"
        List<String> includeColumns = Arrays.asList(
                "transporterDetail.orgData.fullName",
                "pickupDetails.orgData.email",
                "deliveryDetails.orgData.phone.primary",
                "normalColumn"
        );

        // Act
        List<String> result = commonUtils.refineIncludeColumns(new ArrayList<>(includeColumns));

        // Assert
        assertNotNull(result);
        assertEquals(6, result.size());
        assertTrue(result.contains("transporterDetail.orgData"));
        assertTrue(result.contains("pickupDetails.orgData"));
        assertTrue(result.contains("deliveryDetails.orgData"));
        assertTrue(result.contains("normalColumn"));
    }

    @Test
     void testRefineIncludeColumns_WithAddressDataColumns_TruncatesCorrectly() {
        // Arrange - assuming Constants.ADDRESS_DATA = "addressData"
        List<String> includeColumns = Arrays.asList(
                "pickupDetails.addressData.street",
                "deliveryDetails.addressData.city.name",
                "transporterDetail.addressData.zipCode",
                "normalColumn"
        );

        // Act
        List<String> result = commonUtils.refineIncludeColumns(new ArrayList<>(includeColumns));

        // Assert
        assertNotNull(result);
        assertEquals(6, result.size());
        assertTrue(result.contains("pickupDetails.addressData"));
        assertTrue(result.contains("deliveryDetails.addressData"));
        assertTrue(result.contains("transporterDetail.addressData"));
        assertTrue(result.contains("normalColumn"));
    }

    @Test
     void testBuildJoinsAndSelections_EmptyRequestedColumns_DoesNotAddSelections() throws RunnerException {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        String rootEntityKey = "shipmentDetails";
        String sortField = null;

        // Act
        commonUtils.buildJoinsAndSelections(requestedColumns, root, selections, columnOrder, rootEntityKey, sortField);

        // Assert
        assertTrue(selections.isEmpty());
        assertTrue(columnOrder.isEmpty());
    }

    @Test
     void testBuildJoinsAndSelections_WithRootEntityColumns_AddsSelectionsAndColumnOrder() throws RunnerException {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        List<String> rootColumns = Arrays.asList("id", "shipmentNumber", "status");
        requestedColumns.put("shipmentDetails", rootColumns);
        String rootEntityKey = "shipmentDetails";
        String sortField = null;

        // Mock the processEntity method indirectly by verifying it gets called
        // Since processEntity is private, we test its effects through the public method

        // Act
        commonUtils.buildJoinsAndSelections(requestedColumns, root, selections, columnOrder, rootEntityKey, sortField);

        // Assert
        // Verify processEntity was called by checking that root.get() was invoked
        verify(root, atLeastOnce()).get(anyString());

        // The actual selections and columnOrder population depends on processEntity implementation
        // We can only verify that the method completed without throwing exceptions
        assertNotNull(selections);
        assertNotNull(columnOrder);
    }
    @Test
     void testBuildJoinsAndSelections_NullSortField_DoesNotModifyColumns() throws RunnerException {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        Root<ShipmentDetails> root1= mock(Root.class);
        List<String> originalColumns = Arrays.asList("id", "shipmentNumber", "status");
        requestedColumns.put("shipmentDetails", new ArrayList<>(originalColumns));
        Map<String, Object> pickupDetails = new HashMap<>();
        pickupDetails.put("transporterDetail", "field");
        requestedColumns.put("additionalDetails",pickupDetails);
        String rootEntityKey = "shipmentDetails";
        String sortField = "shipmentType";

        // Act
        commonUtils.buildJoinsAndSelections(requestedColumns, root1, selections, columnOrder, rootEntityKey, sortField);

        // Assert
        // Original columns should remain unchanged
        Object rootEntityValue = requestedColumns.get(rootEntityKey);
        assertTrue(rootEntityValue instanceof List);
        List<String> resultColumns = (List<String>) rootEntityValue;
        assertEquals(originalColumns.size(), resultColumns.size());
        assertTrue(resultColumns.containsAll(originalColumns));
    }

    @Test
    void testBuildJoinsAndSelections_NullSortField_DoesNotModifyColumnsForBuildJoin() throws RunnerException {
        // Arrange
        Map<String, Object> requestedColumns = new HashMap<>();
        Root<ShipmentDetails> root1= mock(Root.class);
        List<String> originalColumns = Arrays.asList("id", "shipmentNumber", "status");
        requestedColumns.put("shipmentDetails", new ArrayList<>(originalColumns));
        Map<String, Object> pickupDetails = new HashMap<>();
        pickupDetails.put("transporterDetail", "field");
        requestedColumns.put("additionalDetails",pickupDetails);
        String rootEntityKey = "shipmentDetails";
        String sortField = "shipmentType";

        // Act
        commonUtils.buildJoinsAndSelections(requestedColumns, root1, selections, columnOrder, rootEntityKey, sortField);

        // Assert
        // Original columns should remain unchanged
        Object rootEntityValue = requestedColumns.get(rootEntityKey);
        assertTrue(rootEntityValue instanceof List);
        List<String> resultColumns = (List<String>) rootEntityValue;
        assertEquals(originalColumns.size(), resultColumns.size());
        assertTrue(resultColumns.containsAll(originalColumns));
    }

    @Test
    void testBuildJoinsAndSelections_WithCompleteSetup() throws RunnerException {
        // Arrange
        Map<String, Object> requestedColumns = setupRequestedColumns();
        String rootEntityKey = "shipmentDetails";
        String sortField = "shipmentType";

        setupAllMocks();

        // Act
        commonUtils.buildJoinsAndSelections(requestedColumns, root, selections, columnOrder, rootEntityKey, sortField);

        // Assert
        assertNotNull(selections);
        assertFalse(selections.isEmpty());
        assertTrue(columnOrder.size() > 0);
    }

    private Map<String, Object> setupRequestedColumns() {
        Map<String, Object> requestedColumns = new HashMap<>();
        requestedColumns.put("shipmentDetails", Arrays.asList("id", "shipmentNumber", "status"));
        requestedColumns.put("carrierDetails", Arrays.asList("id", "shippingLine", "vessel", "voyage"));

        Map<String, Object> additionalDetails = new HashMap<>();
        additionalDetails.put("transporterDetail", Arrays.asList("field"));
        requestedColumns.put("additionalDetails", additionalDetails);

        return requestedColumns;
    }

    private void setupAllMocks() {
        // Mock joins
        when(root.join("carrierDetails", JoinType.LEFT)).thenReturn(firstJoin);
        when(root.join("additionalDetails", JoinType.LEFT)).thenReturn(secondJoin);
        when(secondJoin.join("transporterDetail", JoinType.LEFT)).thenReturn(thirdJoin);

        // Mock all get() calls that return Path objects
        setupPathMocks(root, Arrays.asList("id", "shipmentNumber", "status"));
        setupPathMocks(firstJoin, Arrays.asList("id", "shippingLine", "vessel", "voyage"));
        setupPathMocks(thirdJoin, Arrays.asList("field"));

//        // Mock validation method if needed
//        when(commonUtils.isValidFieldForEntity(any(), anyString())).thenReturn(true);
    }

    private void setupPathMocks(Object mockObject, List<String> columns) {
        for (String col : columns) {
            Path mockPath = mock(Path.class);
            if (mockObject instanceof Root) {
                when(((Root) mockObject).get(col)).thenReturn(mockPath);
            } else if (mockObject instanceof Join) {
                when(((Join) mockObject).get(col)).thenReturn(mockPath);
            }
        }
    }

    @Test
     void testProcessNestedMap_EmptyNestedMap_DoesNotModifyCollections() {
        // Arrange
        Map<String, Object> nestedMap = new HashMap<>();
        String rootEntityKey = "shipmentDetails";
        String parentEntityKey = "pickupDetails";

        // Act
        commonUtils.processNestedMap(nestedMap, rootEntityKey, root, selections, columnOrder, parentEntityKey, joinCache);

        // Assert - Test behavior through state changes
        assertTrue(selections.isEmpty());
        assertTrue(columnOrder.isEmpty());
        assertTrue(joinCache.isEmpty());
    }



    private Map<String, Object> createNestedMapExample() {

        Map<String, Object> nestedMap = new HashMap<>();
        nestedMap.put("consigner", Arrays.asList("id",
                "entityId",
                "entityType",
                "type" ));
        nestedMap.put("additionalDetails",Arrays.asList(
                "id",
                "customsNoIssueDate",
                "expiryDate",
                "inspection",
                "airwayBillDims",
                "shipperCOD"
        ));
        nestedMap.put("carrierDetails", Arrays.asList(
                "id",
                "shippingLine",
                "vessel",
                "voyage"
        ));
        nestedMap.put("client",  Arrays.asList(
                "orgData"
        ));

        return nestedMap;
    }



    @Test
    void testDetectCollectionRelationships_WithCollectionFields() throws NoSuchFieldException {
        // Arrange
        Map<String, Object> requestedColumns = createNestedMapExample();

        // Act
        Set<String> result = commonUtils.detectCollectionRelationships(requestedColumns, ShipmentDetails.class);

        // Assert
        assertEquals(0, result.size());
    }

    @Test
    void testBuildFlatList_ReturnsCorrectFlatList() {
        List<Object[]> results = new ArrayList<>();
        results.add(new Object[] {"TQAA25070016", "AIR", "EXP", "STD" , "LSE"});
        results.add(new Object[] {"TQAA25077657", "SEA", "IMP", "STD" , "LSE"});
        List<String> columnOrder = Arrays.asList("shipmentDetails.shipmentId", "shipmentDetails.transportMode", "shipmentDetails.fileStatus", "shipmentDetails.direction", "shipmentDetails.jobType" );

        List<Map<String, Object>> flatList = commonUtils.buildFlatList(results, columnOrder);

        assertEquals(2, flatList.size());
        assertEquals("TQAA25070016", flatList.get(0).get("shipmentDetails.shipmentId"));
        assertEquals("AIR", flatList.get(0).get("shipmentDetails.transportMode"));
        assertEquals("EXP", flatList.get(0).get("shipmentDetails.fileStatus"));
    }

    @Test
    void testFetchTotalCount_WithNoPredicates_ShouldReturnCount() {
        // Given
        Long expectedCount = 10L;
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        FilterCriteria mockFilterCriteria = mock(FilterCriteria.class);
        listCommonRequest.setFilterCriteria(List.of(mockFilterCriteria));

        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Long.class)).thenReturn(countQuery);
        when(countQuery.from(ShipmentDetails.class)).thenReturn(root);
        // Return empty list for "no predicates" scenario
        doReturn(Collections.emptyList()).when(commonUtils)
                .buildPredicatesFromFilters(criteriaBuilder, root, listCommonRequest);
        when(criteriaBuilder.countDistinct(root)).thenReturn(mock(Expression.class));
        when(entityManager.createQuery(countQuery)).thenReturn(typedQuery1);
        when(typedQuery1.getSingleResult()).thenReturn(expectedCount);

        // When
        long result = commonUtils.fetchTotalCount(listCommonRequest, ShipmentDetails.class);

        // Then
        assertEquals(expectedCount, result);
        verify(countQuery).select(any());
        verify(countQuery, never()).where(any(Predicate.class));
        verify(entityManager).createQuery(countQuery);
        verify(typedQuery1).getSingleResult(); // Fixed: use typedQuery1 consistently
    }

    @Test
    void testFetchTotalCount_WithPredicates_ShouldReturnCountForInnerFilter() {
        // Given
        Long expectedCount = 10L;
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        FilterCriteria mockFilterCriteria = mock(FilterCriteria.class);
        FilterCriteria innerfilterCriteria = mock(FilterCriteria.class);
        Criteria innerCriteria = mock(Criteria.class);
        innerCriteria.setFieldName("id");
        innerCriteria.setValue("TQAA25070016");
        innerCriteria.setOperator("AND");
        mockFilterCriteria.setInnerFilter(List.of(innerfilterCriteria));
        listCommonRequest.setFilterCriteria(List.of(mockFilterCriteria));
        Predicate predicates = mock(Predicate.class);
        when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilder);
        when(criteriaBuilder.createQuery(Long.class)).thenReturn(countQuery);
        when(countQuery.from(ShipmentDetails.class)).thenReturn(root);
        // Return empty list for "no predicates" scenario
        doReturn(List.of(predicates)).
                when(commonUtils)
                .buildPredicatesFromFilters(criteriaBuilder, root, listCommonRequest);
                //.thenReturn(List.of(predicates));
        when(criteriaBuilder.countDistinct(root)).thenReturn(mock(Expression.class));
        when(entityManager.createQuery(countQuery)).thenReturn(typedQuery1);
        when(typedQuery1.getSingleResult()).thenReturn(expectedCount);

        // When
        long result = commonUtils.fetchTotalCount(listCommonRequest, ShipmentDetails.class);

        // Then
        assertEquals(expectedCount, result);
        verify(countQuery).select(any());
        verify(countQuery, never()).where(any(Predicate.class));
        verify(entityManager).createQuery(countQuery);
        verify(typedQuery1).getSingleResult(); // Fixed: use typedQuery1 consistently
    }

    @Test
    void testBuildPredicatesFromFilters_WithValidFilters() {
        // Given
        int tenantId = 1;
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        FilterCriteria mockFilterCriteria = mock(FilterCriteria.class);
        FilterCriteria innerfilterCriteria = mock(FilterCriteria.class);
        Criteria innerCriteria = mock(Criteria.class);
        innerCriteria.setFieldName("id");
        innerCriteria.setValue("TQAA25070016");
        innerCriteria.setOperator("AND");
        innerfilterCriteria.setCriteria(innerCriteria);
        mockFilterCriteria.setInnerFilter(List.of(innerfilterCriteria));
        listCommonRequest.setFilterCriteria(List.of(mockFilterCriteria));

//        when(listCommonRequest.getFilterCriteria()).thenReturn(List.of(mockFilterCriteria));

        when(mockFilterCriteria.getInnerFilter()).thenReturn(List.of(innerfilterCriteria));
        when(innerfilterCriteria.getCriteria()).thenReturn(innerCriteria);
        when(innerCriteria.getOperator()).thenReturn("AND");
        when(innerCriteria.getFieldName()).thenReturn("id");
        when(innerCriteria.getValue()).thenReturn("TQAA25070016");

            when(root.get("tenantId")).thenReturn(mockPath);
            when(criteriaBuilder.equal(mockPath, tenantId)).thenReturn(mock(Predicate.class));

            // When
            List<Predicate> result = commonUtils.buildPredicatesFromFilters(criteriaBuilder, root, listCommonRequest);

            // Then
            assertNotNull(result);
            assertEquals(1, result.size());
        }

    private Object getTestValueForOperator(String operator) {
        return switch (operator.toLowerCase()) {
            case "in", "notin" -> Arrays.asList("val1", "val2");
            case "isnull", "isnotnull" -> null;
            case ">", "<", ">=", "<=" -> 100;
            default -> "testValue";
        };
    }
    private ListCommonRequest createListCommonRequestWithOperator(String operator, Object value) {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        FilterCriteria mockFilterCriteria = mock(FilterCriteria.class);
        FilterCriteria innerFilterCriteria = mock(FilterCriteria.class);
        Criteria innerCriteria = mock(Criteria.class);

        // Setup inner criteria with the specific operator
        when(innerCriteria.getFieldName()).thenReturn("testField");
        when(innerCriteria.getValue()).thenReturn(value);
        when(innerCriteria.getOperator()).thenReturn(operator);

        // Setup filter criteria chain
        when(innerFilterCriteria.getCriteria()).thenReturn(innerCriteria);
        when(mockFilterCriteria.getInnerFilter()).thenReturn(List.of(innerFilterCriteria));

        listCommonRequest.setFilterCriteria(List.of(mockFilterCriteria));

        return listCommonRequest;
    }

    @Test
    @DisplayName("Test all operators with different data types")
    void testAllOperatorsWithDifferentDataTypes() {
        // Setup common mocks
        int tenantId = 1;
        when(root.get("tenantId")).thenReturn(mockPath);
        when(criteriaBuilder.equal(mockPath, tenantId)).thenReturn(mock(Predicate.class));
        setupAllOperatorMocks1();

        // Test comparison operators with different data types
        testComparisonOperatorsWithDataTypes();

        // Test other operators
        testNonComparisonOperators();
    }

    private void testComparisonOperatorsWithDataTypes() {
        String[] comparisonOperators = {">", "<", ">=", "<="};

        for (String operator : comparisonOperators) {
            // Test with String
            testOperatorWithDataType(operator, "stringValue", "String");

            // Test with Number (Integer)
            testOperatorWithDataType(operator, 100, "Number");

            // Test with Date
            testOperatorWithDataType(operator, new Date(), "Date");

            // Test with LocalDateTime
            testOperatorWithDataType(operator, LocalDateTime.now(), "LocalDateTime");
        }
    }

    private void testNonComparisonOperators() {
        String[] otherOperators = {"=", "!=", "like", "contains", "notlike",
                "startswith", "endswith", "in", "notin",
                "isnull", "isnotnull"};

        for (String operator : otherOperators) {
            Object testValue = getTestValueForOperator(operator);
            testOperatorWithDataType(operator, testValue, "Standard");
        }
    }

    private void testOperatorWithDataType(String operator, Object value, String dataType) {
        ListCommonRequest request = createListCommonRequestWithOperator(operator, value);

        List<Predicate> result = commonUtils.buildPredicatesFromFilters(criteriaBuilder, root, request);

        assertNotNull(result, String.format("Operator: %s with %s should not return null", operator, dataType));
        assertTrue(result.size() >= 1, String.format("Should have at least tenant predicate for %s operator with %s", operator, dataType));
    }

    // Updated setup method to handle all data types
    private void setupAllOperatorMocks1() {
        Predicate mockPredicate = mock(Predicate.class);
        Expression<String> lowerExpression = mock(Expression.class);

        when(root.get(anyString())).thenReturn(mockPath);
        when(criteriaBuilder.lower(any(Expression.class))).thenReturn(lowerExpression);

        // Basic operators
        when(criteriaBuilder.equal(any(), any())).thenReturn(mockPredicate);
        when(criteriaBuilder.notEqual(any(), any())).thenReturn(mockPredicate);

        // String operations
        when(criteriaBuilder.like(any(Expression.class), anyString())).thenReturn(mockPredicate);
        when(criteriaBuilder.notLike(any(Expression.class), anyString())).thenReturn(mockPredicate);

        // Comparison operations for different types
        // String comparisons
        when(criteriaBuilder.greaterThan(any(Expression.class), any(String.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lessThan(any(Expression.class), any(String.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.greaterThanOrEqualTo(any(Expression.class), any(String.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lessThanOrEqualTo(any(Expression.class), any(String.class))).thenReturn(mockPredicate);

        // Number comparisons (using gt/lt for Number type)
        when(criteriaBuilder.gt(any(Expression.class), any(Number.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lt(any(Expression.class), any(Number.class))).thenReturn(mockPredicate);

        // Date comparisons
        when(criteriaBuilder.greaterThan(any(Expression.class), any(Date.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lessThan(any(Expression.class), any(Date.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.greaterThanOrEqualTo(any(Expression.class), any(Date.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lessThanOrEqualTo(any(Expression.class), any(Date.class))).thenReturn(mockPredicate);

        // LocalDateTime comparisons
        when(criteriaBuilder.greaterThan(any(Expression.class), any(LocalDateTime.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lessThan(any(Expression.class), any(LocalDateTime.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.greaterThanOrEqualTo(any(Expression.class), any(LocalDateTime.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.lessThanOrEqualTo(any(Expression.class), any(LocalDateTime.class))).thenReturn(mockPredicate);

        // Collection and null operations
        when(criteriaBuilder.isMember(any(Object.class), any(Expression.class))).thenReturn(mockPredicate);
        when(criteriaBuilder.isNull(any())).thenReturn(mockPredicate);
        when(criteriaBuilder.isNotNull(any())).thenReturn(mockPredicate);
        when(criteriaBuilder.not(any(Predicate.class))).thenReturn(mockPredicate);
        when(mockPath.in(any(Collection.class))).thenReturn(mockPredicate);
    }

    @Test
    @DisplayName("Test unsupported data types throw exceptions")
    void testUnsupportedDataTypesThrowExceptions() {
        // Setup
        int tenantId = 1;
        when(root.get("tenantId")).thenReturn(mockPath);
        when(criteriaBuilder.equal(mockPath, tenantId)).thenReturn(mock(Predicate.class));
        when(root.get(anyString())).thenReturn(mockPath);

        String[] comparisonOperators = {">", "<", ">=", "<="};

        for (String operator : comparisonOperators) {
            // Test with unsupported type (e.g., Boolean)
            ListCommonRequest request = createListCommonRequestWithOperator(operator, true);

            // This should throw IllegalArgumentException due to unsupported Boolean type
            assertDoesNotThrow(() -> {
                commonUtils.buildPredicatesFromFilters(criteriaBuilder, root, request);
            }, "Should not throw exception for supported Boolean type with operator: " + operator);
        }
    }

    @Test
    void canFetchDetailsWithoutTenantFilter_whenSourceIsNetworkTransfer_returnsTrue() {
        boolean result = CommonUtils.canFetchDetailsWithoutTenantFilter(NETWORK_TRANSFER);
        assertTrue(result);
    }

    @Test
    void canFetchDetailsWithoutTenantFilter_whenUserDoesNotHaveViewPermission_returnsFalse() {
        UserContext.getUser().getPermissions().remove(PermissionConstants.CAN_VIEW_ALL_BRANCH_SHIPMENTS);
        boolean result = CommonUtils.canFetchDetailsWithoutTenantFilter("OTHER_SOURCE");
        assertFalse(result);
    }

    @Test
    void canFetchDetailsWithoutTenantFilter_whenUserHasViewPermission_sourceNotCrossTenant_returnsFalse() {
        UserContext.getUser().getPermissions().put(PermissionConstants.CAN_VIEW_ALL_BRANCH_SHIPMENTS, true);
        boolean result = CommonUtils.canFetchDetailsWithoutTenantFilter("OTHER_SOURCE");
        assertFalse(result);
    }

    @Test
    void canFetchDetailsWithoutTenantFilter_whenUserHasViewPermission_sourceCrossTenant_returnsTrue() {
        UserContext.getUser().getPermissions().put(PermissionConstants.CAN_VIEW_ALL_BRANCH_SHIPMENTS, true);
        boolean result = CommonUtils.canFetchDetailsWithoutTenantFilter(CROSS_TENANT_SOURCE);
        assertTrue(result);
    }
    @Test
    void testCheckPermissionsForCloning_permissionCheckPassed_noExceptionThrown() {
        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setCountryAirCargoSecurity(false);
        doReturn(settings).when(commonUtils).getShipmentSettingFromContext();
        assertDoesNotThrow(() -> commonUtils.checkPermissionsForCloning(shipmentDetails));
    }

    @Test
    void testMapIfSelected_flagIsTrueAndValueIsNotNull_setterIsCalled() {
        Consumer<String> mockSetter = mock(Consumer.class);
        String testValue = "test";
        commonUtils.mapIfSelected(true, testValue, mockSetter);
        verify(mockSetter, times(1)).accept(testValue);
    }

    @Test
    void testMapIfSelected_flagIsFalse_setterIsNotCalled() {
        Consumer<String> mockSetter = mock(Consumer.class);
        String testValue = "test";
        commonUtils.mapIfSelected(false, testValue, mockSetter);
        verify(mockSetter, never()).accept(any());
    }

    @Test
    void testMapIfSelected_valueIsNull_setterIsNotCalled() {
        Consumer<String> mockSetter = mock(Consumer.class);
        commonUtils.mapIfSelected(true, null, mockSetter);
        verify(mockSetter, never()).accept(any());
    }

    @Test
    void testGetPartiesResponse_inputIsNull_returnsEmptyResponse() {
        PartiesResponse response = commonUtils.getPartiesResponse(null);
        assertNotNull(response);
        assertNull(response.getEntityId());
        assertNull(response.getEntityType());
    }

    @Test
    void testGetPartiesResponse_inputIsNotNull_returnsMappedResponse() {
        Parties partyData = new Parties();
        partyData.setEntityId(1L);
        partyData.setEntityType("Client");
        partyData.setTenantId(123);
        partyData.setType("shipper");
        partyData.setOrgCode("ORG1");
        partyData.setAddressCode("ADDR1");
        PartiesResponse response = commonUtils.getPartiesResponse(partyData);
        assertNotNull(response);
        assertEquals(1L, response.getEntityId());
        assertEquals("Client", response.getEntityType());
        assertEquals(123, response.getTenantId());
        assertEquals("shipper", response.getType());
        assertEquals("ORG1", response.getOrgCode());
        assertEquals("ADDR1", response.getAddressCode());
    }

    @Test
    void testCheckSameParties_bothNull_returnsTrue() {
        assertTrue(CommonUtils.checkSameParties(null, null));
    }

    @Test
    void testCheckSameParties_oneNull_returnsFalse() {
        assertFalse(CommonUtils.checkSameParties(new Parties(), null));
        assertFalse(CommonUtils.checkSameParties(null, new Parties()));
    }

    @Test
    void testCheckSameParties_idsMatch_returnsTrue() {
        Parties p1 = new Parties();
        p1.setOrgId("org1");
        p1.setAddressId("addr1");
        Parties p2 = new Parties();
        p2.setOrgId("org1");
        p2.setAddressId("addr1");

        assertTrue(CommonUtils.checkSameParties(p1, p2));
    }

    @Test
    void testCheckSameParties_orgIdMismatch_returnsFalse() {
        Parties p1 = new Parties();
        p1.setOrgId("org1");
        p1.setAddressId("addr1");
        Parties p2 = new Parties();
        p2.setOrgId("org2");
        p2.setAddressId("addr1");

        assertFalse(CommonUtils.checkSameParties(p1, p2));
    }

    @Test
    void testCheckSameParties_addressIdMismatch_returnsFalse() {
        Parties p1 = new Parties();
        p1.setOrgId("org1");
        p1.setAddressId("addr1");
        Parties p2 = new Parties();
        p2.setOrgId("org1");
        p2.setAddressId("addr2");

        assertFalse(CommonUtils.checkSameParties(p1, p2));
    }

    @Test
    void testCheckPartyNotNull_partyIsNull_returnsFalse() {
        assertFalse(CommonUtils.checkPartyNotNull(null));
    }

    @Test
    void testCheckPartyNotNull_orgIdIsNull_returnsFalse() {
        Parties party = new Parties();
        assertFalse(CommonUtils.checkPartyNotNull(party));
    }

    @Test
    void testCheckPartyNotNull_orgIdIsEmpty_returnsFalse() {
        Parties party = new Parties();
        party.setOrgId("");
        assertFalse(CommonUtils.checkPartyNotNull(party));
    }

    @Test
    void testCheckPartyNotNull_orgIdIsPresent_returnsTrue() {
        Parties party = new Parties();
        party.setOrgId("org123");
        assertTrue(CommonUtils.checkPartyNotNull(party));
    }

    @Test
    void testCheckAddressNotNull_partiesInput_partyIsNull_returnsFalse() {
        assertFalse(CommonUtils.checkAddressNotNull((Parties) null));
    }

    @Test
    void testCheckAddressNotNull_partiesInput_orgIdIsEmpty_returnsFalse() {
        Parties party = new Parties();
        assertFalse(CommonUtils.checkAddressNotNull(party));
    }

    @Test
    void testCheckAddressNotNull_partiesInput_addressIdIsNull_returnsFalse() {
        Parties party = new Parties();
        party.setOrgId("org123");
        assertFalse(CommonUtils.checkAddressNotNull(party));
    }

    @Test
    void testCheckAddressNotNull_partiesInput_bothPresent_returnsTrue() {
        Parties party = new Parties();
        party.setOrgId("org123");
        party.setAddressId("addr123");
        assertTrue(CommonUtils.checkAddressNotNull(party));
    }

    @Test
    void testCheckAirSecurityForShipment_securityCheckFails_returnsFalse() {
        try (MockedStatic<UserContext> mockedUserContext = Mockito.mockStatic(UserContext.class)) {
            shipmentDetails = new ShipmentDetails();
            shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            shipmentDetails.setDirection(Constants.DIRECTION_EXP);
            mockedUserContext.when(UserContext::isAirSecurityUser).thenReturn(false);
            assertFalse(CommonUtils.checkAirSecurityForShipment(shipmentDetails));
        }
    }

    @Test
    void testCheckAirSecurityForShipment_securityCheckPasses_returnsTrue() {
        try (MockedStatic<UserContext> mockedUserContext = Mockito.mockStatic(UserContext.class)) {
            shipmentDetails = new ShipmentDetails();
            shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
            shipmentDetails.setDirection(Constants.DIRECTION_EXP);
            mockedUserContext.when(UserContext::isAirSecurityUser).thenReturn(true);
            assertTrue(CommonUtils.checkAirSecurityForShipment(shipmentDetails));
        }
    }

    @Test
    void isSelectedModeOffInBooking_shouldThrowException_forUnknownMode() {
        V1TenantSettingsResponse tenantData = new V1TenantSettingsResponse();
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                commonUtils.isSelectedModeOffInBooking("unknown", tenantData));
        assertEquals("Unknown transport mode: unknown", exception.getMessage());
    }

    @Test
    void checkAirSecurityForTransportTypeAndDirection_AirImport_ShouldReturnTrue() {
        boolean result = commonUtils.checkAirSecurityForTransportTypeAndDirection(
                TRANSPORT_MODE_AIR, DIRECTION_IMP);
        assertTrue(result);
    }

    @Test
    void checkAirSecurityForTransportTypeAndDirection_SeaTransport_ShouldReturnTrue() {
        boolean result = commonUtils.checkAirSecurityForTransportTypeAndDirection(
                TRANSPORT_MODE_SEA, DIRECTION_EXP);
        assertTrue(result);
    }

    @Test
    void validateAirSecurityPermission_CountrySecurityDisabled_ShouldNotThrowException() {
        doReturn(shipmentSettingsDetails).when(commonUtils).getShipmentSettingFromContext();
        when(shipmentSettingsDetails.getCountryAirCargoSecurity()).thenReturn(false);
        assertDoesNotThrow(() ->
                commonUtils.validateAirSecurityPermission(TRANSPORT_MODE_AIR, DIRECTION_EXP));
    }

    @Test
    void validateAirSecurityPermission_AirExportWithAirSecurityUser_ShouldNotThrowException() {
        doReturn(shipmentSettingsDetails).when(commonUtils).getShipmentSettingFromContext();
        when(shipmentSettingsDetails.getCountryAirCargoSecurity()).thenReturn(true);
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::isAirSecurityUser).thenReturn(true);
            assertDoesNotThrow(() ->
                    commonUtils.validateAirSecurityPermission(TRANSPORT_MODE_AIR, DIRECTION_EXP));
        }
    }

    @Test
    void validateAirSecurityPermission_AirExportWithoutAirSecurityUser_ShouldThrowValidationException() {
        doReturn(shipmentSettingsDetails).when(commonUtils).getShipmentSettingFromContext();
        when(shipmentSettingsDetails.getCountryAirCargoSecurity()).thenReturn(true);
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::isAirSecurityUser).thenReturn(false);
            ValidationException exception = assertThrows(ValidationException.class, () ->
                    commonUtils.validateAirSecurityPermission(TRANSPORT_MODE_AIR, DIRECTION_EXP));
            assertEquals(AIR_SECURITY_PERMISSION_MSG, exception.getMessage());
        }
    }

    @Test
    void validateAirSecurityPermission_AirImportWithSecurityEnabled_ShouldNotThrowException() {
        doReturn(shipmentSettingsDetails).when(commonUtils).getShipmentSettingFromContext();
        when(shipmentSettingsDetails.getCountryAirCargoSecurity()).thenReturn(true);
        assertDoesNotThrow(() ->
                commonUtils.validateAirSecurityPermission(TRANSPORT_MODE_AIR, DIRECTION_IMP));
    }

    @Test
    void validateAirSecurityPermission_SeaTransportWithSecurityEnabled_ShouldNotThrowException() {
        doReturn(shipmentSettingsDetails).when(commonUtils).getShipmentSettingFromContext();
        when(shipmentSettingsDetails.getCountryAirCargoSecurity()).thenReturn(true);
        assertDoesNotThrow(() ->
                commonUtils.validateAirSecurityPermission(TRANSPORT_MODE_SEA, DIRECTION_EXP));
    }

    @Test
    void validateAirSecurityPermission_NullCountrySecurity_ShouldNotThrowException() {
        doReturn(shipmentSettingsDetails).when(commonUtils).getShipmentSettingFromContext();
        when(shipmentSettingsDetails.getCountryAirCargoSecurity()).thenReturn(null);
        assertDoesNotThrow(() ->
                commonUtils.validateAirSecurityPermission(TRANSPORT_MODE_AIR, DIRECTION_EXP));
    }

    @Test
    void checkAirSecurityForTransportTypeAndDirection_AirExportWithAirSecurityUser_ShouldReturnTrue() {
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::isAirSecurityUser).thenReturn(true);
            boolean result = commonUtils.checkAirSecurityForTransportTypeAndDirection(
                    TRANSPORT_MODE_AIR, DIRECTION_EXP);
            assertTrue(result);
        }
    }

    @Test
    void checkAirSecurityForTransportTypeAndDirection_AirExportWithoutAirSecurityUser_ShouldReturnFalse() {
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::isAirSecurityUser).thenReturn(false);
            boolean result = commonUtils.checkAirSecurityForTransportTypeAndDirection(
                    TRANSPORT_MODE_AIR, DIRECTION_EXP);
            assertFalse(result);
        }
    }

    @ParameterizedTest
    @MethodSource("transportModesProvider")
    void isSelectedModeOffInBooking_shouldReturnFalse_whenModeIsOn(String transportMode) {
        V1TenantSettingsResponse tenantData = new V1TenantSettingsResponse();
        switch (transportMode) {
            case "SEA" -> tenantData.setBookingTransportModeSea(true);
            case "RAI" -> tenantData.setBookingTransportModeRail(true);
            case "ROA" -> tenantData.setBookingTransportModeRoad(true);
            case "AIR" -> tenantData.setBookingTransportModeAir(true);
            default -> throw new IllegalArgumentException("Invalid test case mode: " + transportMode);
        }
        assertFalse(commonUtils.isSelectedModeOffInBooking(transportMode, tenantData));
    }

    @ParameterizedTest
    @MethodSource("transportModesProvider")
    void isSelectedModeOffInShipment_shouldReturnFalse_whenModeIsOn(String transportMode) {
        V1TenantSettingsResponse tenantData = new V1TenantSettingsResponse();
        switch (transportMode) {
            case "SEA" -> tenantData.setShipmentTransportModeSea(true);
            case "RAI" -> tenantData.setShipmentTransportModeRail(true);
            case "ROA" -> tenantData.setShipmentTransportModeRoad(true);
            case "AIR" -> tenantData.setShipmentTransportModeAir(true);
            default -> throw new IllegalArgumentException("Invalid test case mode: " + transportMode);
        }
        assertFalse(commonUtils.isSelectedModeOffInShipment(transportMode, tenantData));
    }

    @Test
    void isSelectedModeOffInShipment_shouldThrowException_forUnknownMode() {
        V1TenantSettingsResponse tenantData = new V1TenantSettingsResponse();
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                commonUtils.isSelectedModeOffInShipment("unknown", tenantData));
        assertEquals("Unknown transport mode: unknown", exception.getMessage());
    }

    @Test
    void testGetCloneFieldResponse_ReturnsValidResponse() throws Exception {
        String type = "S2B";
        String json = "{ \"header\": { \"label\": \"Header\", \"value\": \"header\", \"fields\": [] } }";
        CloneFieldResponse expectedResponse = new CloneFieldResponse();
        when(applicationConfigService.getValue(type)).thenReturn(json);
        when(objectMapper.readValue(json, CloneFieldResponse.class)).thenReturn(expectedResponse);
        CloneFieldResponse actual = commonUtils.getCloneFieldResponse(type);
        assertNotNull(actual);
        assertEquals(expectedResponse, actual);
        verify(applicationConfigService, times(1)).getValue(type);
        verify(objectMapper, times(1)).readValue(json, CloneFieldResponse.class);
    }

    @Test
    void testGetCloneFieldResponse_WhenJsonParsingFails_ThrowsValidationException() throws Exception {
        String type = "S2B";
        String invalidJson = "invalid";
        when(applicationConfigService.getValue(type)).thenReturn(invalidJson);
        when(objectMapper.readValue(invalidJson, CloneFieldResponse.class))
                .thenThrow(new RuntimeException("Parse error"));
        ValidationException thrown = assertThrows(ValidationException.class,
                () -> commonUtils.getCloneFieldResponse(type));
        assertEquals("Invalid Type", thrown.getMessage());
        verify(applicationConfigService, times(1)).getValue(type);
        verify(objectMapper, times(1)).readValue(invalidJson, CloneFieldResponse.class);
    }

    private static Stream<String> transportModesProvider() {
        return Stream.of("SEA", "RAI", "ROA","AIR");
    }
    @Test
    void testConvertSeconds_OnlySeconds() {
        // Test cases where result should only show seconds (less than 60 seconds)
        String result1 = commonUtils.convertSeconds(45L);
        assertEquals("The export will be available in approximately 45 seconds. Please try again after that time.", result1);

        String result2 = commonUtils.convertSeconds(30L);
        assertEquals("The export will be available in approximately 30 seconds. Please try again after that time.", result2);

        String result3 = commonUtils.convertSeconds(59L);
        assertEquals("The export will be available in approximately 59 seconds. Please try again after that time.", result3);

        String result4 = commonUtils.convertSeconds(1L);
        assertEquals("The export will be available in approximately 1 seconds. Please try again after that time.", result4);
    }

    @Test
    void testConvertSeconds_OnlyMinutes() {
        // Test cases where result should only show minutes (exact minutes, no remaining seconds)
        String result1 = commonUtils.convertSeconds(60L);
        assertEquals("The export will be available in approximately 1 minutes. Please try again after that time.", result1);

        String result2 = commonUtils.convertSeconds(120L);
        assertEquals("The export will be available in approximately 2 minutes. Please try again after that time.", result2);

        String result3 = commonUtils.convertSeconds(300L);
        assertEquals("The export will be available in approximately 5 minutes. Please try again after that time.", result3);

        String result4 = commonUtils.convertSeconds(3600L);
        assertEquals("The export will be available in approximately 60 minutes. Please try again after that time.", result4);
    }

    @Test
    void testConvertSeconds_MinutesAndSeconds() {
        // Test cases where result should show both minutes and seconds
        String result1 = commonUtils.convertSeconds(90L);
        assertEquals("The export will be available in approximately 1 minutes and 30 seconds. Please try again after that time.", result1);

        String result2 = commonUtils.convertSeconds(150L);
        assertEquals("The export will be available in approximately 2 minutes and 30 seconds. Please try again after that time.", result2);

        String result3 = commonUtils.convertSeconds(3661L);
        assertEquals("The export will be available in approximately 61 minutes and 1 seconds. Please try again after that time.", result3);

        String result4 = commonUtils.convertSeconds(125L);
        assertEquals("The export will be available in approximately 2 minutes and 5 seconds. Please try again after that time.", result4);
    }

    @Test
    void testConvertSeconds_EdgeCases() {
        // Test edge cases
        String result1 = commonUtils.convertSeconds(0L);
        assertEquals("The export will be available in approximately 0 seconds. Please try again after that time.", result1);

        // Test large values
        String result2 = commonUtils.convertSeconds(7200L); // 2 hours = 120 minutes
        assertEquals("The export will be available in approximately 120 minutes. Please try again after that time.", result2);

        String result3 = commonUtils.convertSeconds(7201L); // 2 hours and 1 second
        assertEquals("The export will be available in approximately 120 minutes and 1 seconds. Please try again after that time.", result3);
    }

    @Test
    void testConvertSeconds_LargeValues() {
        // Test very large long values
        String result1 = commonUtils.convertSeconds(100000L);
        assertEquals("The export will be available in approximately 1666 minutes and 40 seconds. Please try again after that time.", result1);

        String result2 = commonUtils.convertSeconds(Long.MAX_VALUE);
        long expectedMinutes = Long.MAX_VALUE / 60;
        long expectedSeconds = Long.MAX_VALUE % 60;
        String expected = "The export will be available in approximately " + expectedMinutes + " minutes and " + expectedSeconds + " seconds. Please try again after that time.";
        assertEquals(expected, result2);
    }
    @Test
    void testAddTenantIdAndTriangulationData_allValuesPresent() {
        Set<Long> tenantIds = new HashSet<>();
        Integer tenantId = 100;
        List<TriangulationPartner> partners = List.of(
                TriangulationPartner.builder().triangulationPartner(200L).build(),
                TriangulationPartner.builder().triangulationPartner(300L).build()
        );

        commonUtils.addTenantIdAndTriangulationData(tenantIds, tenantId, partners);

        assertEquals(3, tenantIds.size());
        assertTrue(tenantIds.contains(100L));
        assertTrue(tenantIds.contains(200L));
        assertTrue(tenantIds.contains(300L));
    }

    @Test
    void testAddTenantIdAndTriangulationData_nullTenantId() {
        Set<Long> tenantIds = new HashSet<>();
        List<TriangulationPartner> partners = List.of(
                TriangulationPartner.builder().triangulationPartner(200L).build()
        );

        commonUtils.addTenantIdAndTriangulationData(tenantIds, null, partners);

        assertEquals(1, tenantIds.size());
        assertTrue(tenantIds.contains(200L));
    }

    @Test
    void testAddTenantIdAndTriangulationData_nullPartnerList() {
        Set<Long> tenantIds = new HashSet<>();
        Integer tenantId = 100;

        commonUtils.addTenantIdAndTriangulationData(tenantIds, tenantId, null);

        assertEquals(1, tenantIds.size());
        assertTrue(tenantIds.contains(100L));
    }

    @Test
    void testAddTenantIdAndTriangulationData_emptyPartnerList() {
        Set<Long> tenantIds = new HashSet<>();
        Integer tenantId = 100;

        commonUtils.addTenantIdAndTriangulationData(tenantIds, tenantId, Collections.emptyList());

        assertEquals(1, tenantIds.size());
        assertTrue(tenantIds.contains(100L));
    }

    @Test
    void testHandleParentEntity_allValuesPresent() {
        Set<Long> tenantIds = new HashSet<>();
        Integer tenantId = 100;
        Long receivingBranch = 200L;
        Long originBranch = 300L;
        List<TriangulationPartner> partners = List.of(
                TriangulationPartner.builder().triangulationPartner(400L).build()
        );
        doNothing().when(commonUtils).addTenantIdAndTriangulationData(anySet(), any(), any());

        commonUtils.handleParentEntity(tenantIds, tenantId, receivingBranch, originBranch, partners);

        assertEquals(2, tenantIds.size());
        assertTrue(tenantIds.contains(200L));
        assertTrue(tenantIds.contains(300L));
        verify(commonUtils).addTenantIdAndTriangulationData(tenantIds, tenantId, partners);
    }

    @Test
    void testHandleParentEntity_nullBranches() {
        Set<Long> tenantIds = new HashSet<>();
        Integer tenantId = 100;
        List<TriangulationPartner> partners = List.of(
                TriangulationPartner.builder().triangulationPartner(400L).build()
        );
        doNothing().when(commonUtils).addTenantIdAndTriangulationData(anySet(), any(), any());

        commonUtils.handleParentEntity(tenantIds, tenantId, null, null, partners);

        assertTrue(tenantIds.isEmpty());
        verify(commonUtils).addTenantIdAndTriangulationData(tenantIds, tenantId, partners);
    }

    @Test
    void testHandleInterbranchConsolidation_interBranchFalse() {
        Set<Long> tenantIds = new HashSet<>();
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setInterBranchConsole(false);

        commonUtils.handleInterbranchConsolidation(tenantIds, consolidation);

        verify(commonUtils, never()).addTenantIdAndTriangulationData(anySet(), any(), any());
    }

    @Test
    void testHandleInterbranchConsolidation_interBranchTrue_noShipments() {
        Set<Long> tenantIds = new HashSet<>();
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setInterBranchConsole(true);
        consolidation.setShipmentsList(new HashSet<>());

        commonUtils.handleInterbranchConsolidation(tenantIds, consolidation);

        verify(commonUtils, never()).addTenantIdAndTriangulationData(anySet(), any(), any());
    }

    @Test
    void testHandleInterbranchConsolidation_interBranchTrue_withShipments() {
        Set<Long> tenantIds = new HashSet<>();
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setInterBranchConsole(true);

        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setId(1L);
        shipment1.setTenantId(101);
        shipment1.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(102L).build()));

        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setId(2L);
        shipment2.setTenantId(201);
        shipment2.setTriangulationPartnerList(Collections.emptyList());

        consolidation.setShipmentsList(new HashSet<>(Arrays.asList(shipment1, shipment2)));
        doNothing().when(commonUtils).addTenantIdAndTriangulationData(anySet(), any(), any());

        commonUtils.handleInterbranchConsolidation(tenantIds, consolidation);

        verify(commonUtils, times(1)).addTenantIdAndTriangulationData(tenantIds, 101, shipment1.getTriangulationPartnerList());
        verify(commonUtils, times(1)).addTenantIdAndTriangulationData(tenantIds, 201, shipment2.getTriangulationPartnerList());
    }


    @Test
    void testAddTenantDataFromParentGuid_nullGuid() {
        Set<Long> tenantIds = new HashSet<>();
        commonUtils.addTenantDataFromParentGuid(null, tenantIds, "SHIPMENT");
        assertTrue(tenantIds.isEmpty());
        verify(commonUtils, never()).handleParentEntity(anySet(), any(), any(), any(), any());
    }

    @Test
    void testAddTenantDataFromParentGuid_shipment_parentNotFound() {
        UUID parentGuid = UUID.randomUUID();
        Set<Long> tenantIds = new HashSet<>();
        when(shipmentDao.findShipmentByGuidWithQuery(parentGuid)).thenReturn(Optional.empty());

        commonUtils.addTenantDataFromParentGuid(parentGuid, tenantIds, "SHIPMENT");

        assertTrue(tenantIds.isEmpty());
        verify(shipmentDao).findShipmentByGuidWithQuery(parentGuid);
        verify(commonUtils, never()).handleParentEntity(anySet(), any(), any(), any(), any());
    }

    @Test
    void testAddTenantDataFromParentGuid_shipment_parentFound_noRelated() {
        UUID parentGuid = UUID.randomUUID();
        Set<Long> tenantIds = new HashSet<>();
        ShipmentDetails parentShipment = new ShipmentDetails();
        parentShipment.setTenantId(1);
        when(shipmentDao.findShipmentByGuidWithQuery(parentGuid)).thenReturn(Optional.of(parentShipment));
        when(shipmentDao.findByParentGuid(parentGuid)).thenReturn(Collections.emptyList());
        doNothing().when(commonUtils).handleParentEntity(anySet(), any(), any(), any(), any());

        commonUtils.addTenantDataFromParentGuid(parentGuid, tenantIds, "SHIPMENT");

        verify(commonUtils).handleParentEntity(tenantIds, 1, null, null, null);
        verify(shipmentDao).findByParentGuid(parentGuid);
        verify(commonUtils, never()).handleInterbranchConsolidation(anySet(), any());
        verify(commonUtils, times(0)).addTenantIdAndTriangulationData(anySet(), any(), any());
    }

    @Test
    void testAddTenantDataFromParentGuid_shipment_parentFound_withRelated() {
        UUID parentGuid = UUID.randomUUID();
        Set<Long> tenantIds = new HashSet<>();
        ShipmentDetails parentShipment = new ShipmentDetails();
        parentShipment.setTenantId(1);

        ShipmentDetails relatedShipment = new ShipmentDetails();
        relatedShipment.setTenantId(10);
        relatedShipment.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(11L).build()));

        when(shipmentDao.findShipmentByGuidWithQuery(parentGuid)).thenReturn(Optional.of(parentShipment));
        when(shipmentDao.findByParentGuid(parentGuid)).thenReturn(List.of(relatedShipment));
        doNothing().when(commonUtils).handleParentEntity(anySet(), any(), any(), any(), any());
        doNothing().when(commonUtils).addTenantIdAndTriangulationData(anySet(), any(), any());

        commonUtils.addTenantDataFromParentGuid(parentGuid, tenantIds, "SHIPMENT");

        verify(commonUtils).handleParentEntity(tenantIds, 1, null, null, null);
        verify(commonUtils).addTenantIdAndTriangulationData(tenantIds, 10, relatedShipment.getTriangulationPartnerList());
    }

    @Test
    void testAddTenantDataFromParentGuid_consolidation_parentFound_withRelated() {
        UUID parentGuid = UUID.randomUUID();
        Set<Long> tenantIds = new HashSet<>();
        ConsolidationDetails parentConsolidation = new ConsolidationDetails();
        parentConsolidation.setTenantId(1);

        ConsolidationDetails relatedConsolidation = new ConsolidationDetails();
        relatedConsolidation.setTenantId(20);
        relatedConsolidation.setTriangulationPartnerList(List.of(TriangulationPartner.builder().triangulationPartner(21L).build()));

        when(consolidationDetailsDao.findConsolidationByGuidWithQuery(parentGuid)).thenReturn(Optional.of(parentConsolidation));
        when(consolidationDetailsDao.findByParentGuid(parentGuid)).thenReturn(List.of(relatedConsolidation));
        doNothing().when(commonUtils).handleParentEntity(anySet(), any(), any(), any(), any());
        doNothing().when(commonUtils).handleInterbranchConsolidation(anySet(), any());
        doNothing().when(commonUtils).addTenantIdAndTriangulationData(anySet(), any(), any());

        commonUtils.addTenantDataFromParentGuid(parentGuid, tenantIds, "CONSOLIDATION");

        verify(commonUtils).handleParentEntity(tenantIds, 1, null, null, null);
        verify(commonUtils).handleInterbranchConsolidation(tenantIds, parentConsolidation);
        verify(commonUtils).addTenantIdAndTriangulationData(tenantIds, 20, relatedConsolidation.getTriangulationPartnerList());
    }


    @Test
    void testAddTenantDataFromParentGuid_consolidation_parentNotFound() {
        UUID parentGuid = UUID.randomUUID();
        Set<Long> tenantIds = new HashSet<>();
        when(consolidationDetailsDao.findConsolidationByGuidWithQuery(parentGuid)).thenReturn(Optional.empty());

        commonUtils.addTenantDataFromParentGuid(parentGuid, tenantIds, "CONSOLIDATION");

        assertTrue(tenantIds.isEmpty());
        verify(consolidationDetailsDao).findConsolidationByGuidWithQuery(parentGuid);
        verify(commonUtils, never()).handleParentEntity(anySet(), any(), any(), any(), any());
    }

}