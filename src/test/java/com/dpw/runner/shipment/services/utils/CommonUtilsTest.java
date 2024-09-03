package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.v1.response.CoLoadingMAWBDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantDetailsByListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
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
import org.mockito.*;
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
import java.math.BigDecimal;
import java.util.List;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.andCriteria;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
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
    private ShipmentService shipmentService;

    @Mock
    private ICarrierDetailsDao carrierDetailsDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    private PdfContentByte dc;
    private BaseFont font;
    private Rectangle realPageSize;
    private Rectangle rect;
    private PdfReader reader;
    private PdfStamper stamper;
    private ByteArrayOutputStream outputStream;
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
        when(consolidationDetails.getAchievedQuantities()).thenReturn(achievedQuantities);
        when(achievedQuantities.getWeightUtilization()).thenReturn("150");
        when(achievedQuantities.getVolumeUtilization()).thenReturn("50");
        commonUtils.updateConsolOpenForAttachment(consolidationDetails);
        verify(consolidationDetails, times(1)).setOpenForAttachment(false);
    }

    @Test
    void testUpdateConsolOpenForAttachment_withVolumeUtilizationAbove100() {
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
        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        unlocationsMap.put("test", new UnlocationsResponse());
        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsMap);
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
                "username");
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
                null);
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
                null);
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
                "username");
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
                "username");
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
                null);
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
                null);
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
                "username");
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
                "username");
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
                null);
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
                null);
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
                "username");
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
                "username");
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
                null);
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
                null);
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
                "username");
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
                "username");
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
                null);
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
                null);
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
                "username");
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
                "username");
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
                null);
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
                "username");
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
                "username");
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
        when(iv1Service.getEmailTemplates(any())).thenReturn(V1DataResponse.builder().entities(emailTemplatesRequests).build());
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(emailTemplatesRequests);
        Map<ShipmentRequestedType, EmailTemplatesRequest> response = new HashMap<>();
        commonUtils.getEmailTemplate(response);
        assertEquals(6, response.size());
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

    private Runnable mockRunnable() {
        return () -> {};
    }

}