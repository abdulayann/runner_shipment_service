package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.AuditLogDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ExcelUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.PageImpl;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AuditLogServiceTest {

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private ExcelUtils excelUtils;

    @Mock
    private AuditLogDao auditLogDao;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private AuditLogService auditLogService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails testShipment;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setUp() {
        testShipment = jsonTestUtility.getTestShipment();
        MDC.put("skip-audit-log", "false");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
    }
    @Test
    void testDownloadExcel() {
        assertThrows(RunnerException.class, () -> auditLogService.downloadExcel(null));
    }

    @Test
    void testDownloadExcel2() throws RunnerException {
        JsonNodeFactory factory = JsonNodeFactory.instance;
        ObjectNode jsonNode = factory.objectNode();
        ObjectNode innerNode = factory.objectNode();
        innerNode.put("oldValue", 1);
        innerNode.put("newValue", 2);
        innerNode.put("fieldName", "Id");
        jsonNode.set("Id", innerNode);
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("Id");
        auditLogChanges.setNewValue(2);
        auditLogChanges.setOldValue(1);
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("Id", auditLogChanges);
        auditLog.setEntityId(1L);
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        when(excelUtils.createExcelAsResource(any(), any(), any())).thenReturn(null);
        when(jsonHelper.convertValue(any(), eq(JsonNode.class))).thenReturn(jsonNode);
        Resource result = auditLogService.downloadExcel(CommonRequestModel.builder().data(new ListCommonRequest()).build());
        assertNull(result);
    }

    @Test
    void testDownloadExcel3() throws RunnerException {
        JsonNodeFactory factory = JsonNodeFactory.instance;
        ObjectNode jsonNode = factory.objectNode();
        ObjectNode innerNode = factory.objectNode();
        innerNode.put("oldValue", 1);
        innerNode.put("newValue", 2);
        innerNode.put("fieldName", "Id");
        jsonNode.set("Id", innerNode);
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("Id");
        auditLogChanges.setNewValue(2);
        auditLogChanges.setOldValue(1);
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("Id", auditLogChanges);
        auditLog.setEntityId(1L);
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        when(jsonHelper.convertValue(any(), eq(JsonNode.class))).thenReturn(jsonNode);
        doThrow(new RunnerException()).when(excelUtils).createExcelAsResource(any(), any(), any());
        assertThrows(RunnerException.class, () -> auditLogService.downloadExcel(CommonRequestModel.builder().data(new ListCommonRequest()).build()));
    }

    @Test
    void testAddAuditLog() {
        assertThrows(RunnerException.class, () -> auditLogService.addAuditLog(new AuditLogMetaData()));
    }

    @Test
    void testAddAuditLog2() {
        BaseEntity newData = new BaseEntity();
        newData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setCreatedBy("abc");
        newData.setGuid(UUID.randomUUID());
        newData.setId(1L);
        newData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setUpdatedBy("def");
        AuditLogMetaData.AuditLogMetaDataBuilder parentIdResult = AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                .newData(newData)
                .operation("Operation")
                .parent("Parent");

        BaseEntity prevData = new BaseEntity();
        prevData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setCreatedBy("abc");
        prevData.setGuid(UUID.randomUUID());
        prevData.setId(1L);
        prevData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setUpdatedBy("def");
        AuditLogMetaData auditLogMetaData = parentIdResult.prevData(prevData).build();

        assertThrows(RunnerException.class, () -> auditLogService.addAuditLog(auditLogMetaData));
    }

    @Test
    void testAddAuditLog3() {
        AuditLogMetaData.AuditLogMetaDataBuilder auditLogMetaDataBuilder = mock(
                AuditLogMetaData.AuditLogMetaDataBuilder.class);
        when(auditLogMetaDataBuilder.newData(any())).thenReturn(AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username));

        BaseEntity newData = new BaseEntity();
        newData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setCreatedBy("abc");
        newData.setGuid(UUID.randomUUID());
        newData.setId(1L);
        newData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setUpdatedBy("def");
        AuditLogMetaData.AuditLogMetaDataBuilder parentIdResult = auditLogMetaDataBuilder.newData(newData)
                .operation("Operation")
                .parent("Parent")
                .parentId(1L);

        BaseEntity prevData = new BaseEntity();
        prevData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setCreatedBy("abc");
        prevData.setGuid(UUID.randomUUID());
        prevData.setId(1L);
        prevData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setUpdatedBy("def");
        AuditLogMetaData auditLogMetaData = parentIdResult.prevData(prevData).build();

        assertThrows(IllegalArgumentException.class, () -> auditLogService.addAuditLog(auditLogMetaData));
        verify(auditLogMetaDataBuilder).newData(isA(BaseEntity.class));
    }

    @Test
    void addAuditLog4() {
        AuditLogMetaData auditLogMetaData = new AuditLogMetaData();
        auditLogMetaData.setParent("S");
        auditLogMetaData.setParentId(1L);
        Exception e = assertThrows(RunnerException.class, () -> auditLogService.addAuditLog(auditLogMetaData));

        String errorMessage ="Data is missing for ops " + auditLogMetaData.getOperation();
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void addAuditLog5() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails newData = new ShipmentDetails();
        newData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setCreatedBy("abc");
        newData.setGuid(UUID.randomUUID());
        newData.setId(1L);
        newData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setUpdatedBy("def");
        newData.setVolume(BigDecimal.ONE);
        newData.setWeight(BigDecimal.ZERO);
        newData.setNetWeight(BigDecimal.ZERO);
        newData.setHouseBill("1234");
        newData.setCarrierDetails(CarrierDetails.builder().origin("123").build());
        newData.setShipmentCompletedOn(LocalDateTime.MIN);
        newData.setShipmentType("abcd");

        ShipmentDetails prevData = new ShipmentDetails();
        prevData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setCreatedBy("abc");
        prevData.setGuid(UUID.randomUUID());
        prevData.setId(1L);
        prevData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setUpdatedBy("def");
        prevData.setVolume(BigDecimal.TEN);
        prevData.setWeight(BigDecimal.ZERO);
        prevData.setVolumetricWeight(BigDecimal.ZERO);
        prevData.setHouseBill("222");
        prevData.setShipmentCompletedOn(LocalDateTime.now());
        prevData.setDirection("EXP");
        AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username).prevData(prevData).newData(newData).parent("Shipment").operation("UPDATE").parentId(1L).build();
        auditLogService.addAuditLog(auditLogMetaData);
        verify(auditLogDao, times(1)).save(any());
    }

    @Test
    void addAuditLog6() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails newData = testShipment;
        newData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setCreatedBy("abc");
        newData.setGuid(UUID.randomUUID());
        newData.setId(1L);
        newData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setUpdatedBy("def");

        AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username).prevData(null).newData(newData).parent("Shipment").operation("CREATE").parentId(1L).build();
        auditLogService.addAuditLog(auditLogMetaData);
        verify(auditLogDao, times(1)).save(any());
    }

    @Test
    void addAuditLog6_1() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username).prevData(null).newData(MblDuplicatedLog.builder().build()).parent("Shipment").operation("LOG").parentId(1L).build();
        auditLogService.addAuditLog(auditLogMetaData);
        verify(auditLogDao, times(1)).save(any());
    }

    @Test
    void addAuditLog7() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails newData = new ShipmentDetails();
        newData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setCreatedBy("abc");
        newData.setId(1L);
        newData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        newData.setUpdatedBy("def");

        ShipmentDetails prevData = new ShipmentDetails();
        prevData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setCreatedBy("abc");
        prevData.setId(1L);
        prevData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setUpdatedBy("def");

        AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username).prevData(prevData).newData(newData).parent("Shipment").operation("UPDATE").parentId(1L).build();
        auditLogService.addAuditLog(auditLogMetaData);
        verify(auditLogDao, times(0)).save(any());
    }

    @Test
    void addAuditLog8() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        ShipmentDetails prevData = testShipment;
        prevData.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setCreatedBy("abc");
        prevData.setGuid(UUID.randomUUID());
        prevData.setId(1L);
        prevData.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        prevData.setUpdatedBy("def");
        AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username).prevData(prevData).newData(null).parent("Shipment").operation("DELETE").parentId(1L).build();
        auditLogService.addAuditLog(auditLogMetaData);
        verify(auditLogDao, times(1)).save(any());
    }

    @Test
    void testList() {
        var response = auditLogService.list(null, null);
        assertNotNull(response);
    }

    @Test
    void testList2() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("houseBill");
        auditLogChanges.setNewValue("2");
        auditLogChanges.setOldValue("1");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("houseBill", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("ShipmentDetails");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testListNTE() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("houseBill");
        auditLogChanges.setNewValue("2");
        auditLogChanges.setOldValue("1");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("houseBill", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("ShipmentDetails");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAllWithoutTenantFilter(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), Constants.NETWORK_TRANSFER);
        assertNotNull(response);
    }

    @Test
    void testList3() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("sealNumber");
        auditLogChanges.setNewValue("2");
        auditLogChanges.setOldValue("1");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("sealNumber", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("Containers");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList4() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("origin");
        auditLogChanges.setNewValue("2");
        auditLogChanges.setOldValue("1");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("origin", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("Packing");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList5() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("mode");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("mode", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("Routings");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList6() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("text");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("text", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("Notes");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList7() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("vessel");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("vessel", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("BookingCarriage");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList8() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("eventCode");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("eventCode", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("Events");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList9() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("type");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("type", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("ReferenceNumbers");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList10() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("type");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("type", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("Parties");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList11() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("serviceType");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("serviceType", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("ServiceDetails");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), null);
        assertNotNull(response);
    }

    @Test
    void testList12() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("transporterType");
        auditLogChanges.setNewValue("SEA");
        auditLogChanges.setOldValue("AIR");
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("transporterType", auditLogChanges);
        auditLog.getChanges().put("Id", new AuditLogChanges("Id", 1, 2));
        auditLog.setEntityId(1L);
        auditLog.setEntity("TruckDriverDetails");
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        when(auditLogDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(auditLog)));
        var response = auditLogService.list(CommonRequestModel.builder().data(new ListCommonRequest()).build(), "");
        assertNotNull(response);
    }

    @Test
    void testList13() {
        AuditLog auditLog = new AuditLog();
        AuditLogChanges auditLogChanges = new AuditLogChanges();
        auditLogChanges.setFieldName("Id");
        auditLogChanges.setNewValue(2);
        auditLogChanges.setOldValue(1);
        auditLog.setId(1L);
        auditLog.setChanges(new HashMap<>());
        auditLog.getChanges().put("Id", auditLogChanges);
        auditLog.setEntityId(1L);
        auditLog.setParentType("ShipmentDetails");
        auditLog.setCreatedAt(LocalDateTime.now());
        var response = auditLogService.list(CommonRequestModel.builder().data(null).build(), Constants.NETWORK_TRANSFER);
        assertNotNull(response);
    }
}
