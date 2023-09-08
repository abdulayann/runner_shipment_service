package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.AuditLogResponse;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.ExcelUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.persistence.*;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class AuditLogService implements IAuditLogService {
    private static final Set<Class<?>> annotationClassList = new HashSet<>(Arrays.asList(Id.class, OneToOne.class, OneToMany.class, ManyToOne.class, ManyToMany.class));

    public static Map<String, String> COLUMN_HEADERS_TO_FIELD_NAME = null;

    static {
        COLUMN_HEADERS_TO_FIELD_NAME = new LinkedHashMap<>();
        COLUMN_HEADERS_TO_FIELD_NAME.put("Module", "moduleTypeCode");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Module id", "moduleId");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Field Name", "fieldName");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Old value", "oldValue");
        COLUMN_HEADERS_TO_FIELD_NAME.put("New value", "newValue");
        COLUMN_HEADERS_TO_FIELD_NAME.put("User Name", "createdBy");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Changed Date", "createdAt");
    }

    @Autowired
    private IAuditLogDao auditLogDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ExcelUtils excelUtils;

    public Resource downloadExcel(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            var triplet = fetchList(commonRequestModel);
            List<Map<String, Object>> listAsMap = getData(triplet.getLeft());
            Resource fileResource = excelUtils.createExcelAsResource(listAsMap, COLUMN_HEADERS_TO_FIELD_NAME, "Audit_Logs");
            return fileResource;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw e;
        }
    }

    private List<Map<String, Object>> getData(List<IRunnerResponse> auditLogResponses) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (IRunnerResponse item : auditLogResponses) {
            AuditLogResponse ct = (AuditLogResponse) item;
            Map<String, Object> asMap = new HashMap<>();
            asMap.put("moduleTypeCode", ct.getEntity());
            asMap.put("moduleId", ct.getEntityId().toString());
            asMap.put("createdBy", ct.getCreatedBy());
            asMap.put("createdAt", DateUtils.getDateAsString(ct.getCreatedAt()));
            //field name, old value, new value
            JsonNode nodeChanges = jsonHelper.convertValue(ct.getChanges(), JsonNode.class);
            Iterator<String> fieldNames = nodeChanges.fieldNames();
            while (fieldNames.hasNext()) {
                Map<String, Object> clone = new HashMap<>();
                clone.putAll(asMap);
                String field = fieldNames.next();
                clone.put("fieldName", field);
                clone.put("oldValue", nodeChanges.get(field).get("oldValue").asText());
                clone.put("newValue", nodeChanges.get(field).get("newValue").asText());
                result.add(clone);
            }
        }
        return result;
    }

    public void addAuditLog(AuditLogMetaData auditLogMetaData) throws IllegalAccessException, NoSuchFieldException, JsonProcessingException {
        validateRequest(auditLogMetaData);

        AuditLog auditLog = new AuditLog();
        auditLog.setOperation(auditLogMetaData.getOperation());
        auditLog.setParentType(auditLogMetaData.getParent());
        auditLog.setParentId(auditLogMetaData.getParentId());

        String ops = auditLogMetaData.getOperation();

        if (ops.equals(DBOperationType.CREATE.name())) {
            auditLog.setChanges(getChanges(auditLogMetaData.getNewData(), null, ops));
            auditLog.setEntity(auditLogMetaData.getNewData().getClass().getSimpleName());
            auditLog.setEntityId(getEntityId(auditLogMetaData.getNewData()));
        } else if (ops.equals(DBOperationType.UPDATE.name())) {
            auditLog.setChanges(getChanges(auditLogMetaData.getNewData(), auditLogMetaData.getPrevData(), ops));
            auditLog.setEntity(auditLogMetaData.getNewData().getClass().getSimpleName());
            auditLog.setEntityId(getEntityId(auditLogMetaData.getNewData()));
        } else if (ops.equals(DBOperationType.DELETE.name())) {
            auditLog.setChanges(getChanges(null, auditLogMetaData.getPrevData(), ops));
            auditLog.setEntity(auditLogMetaData.getPrevData().getClass().getSimpleName());
            auditLog.setEntityId(getEntityId(auditLogMetaData.getPrevData()));
        } else {
            throw new RunnerException("Not a valid operation performed");
        }

        auditLogDao.save(auditLog);
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
//            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
//            if(request == null) {
//                log.error("Request is empty for audit log list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
//            }
//            Pair<Specification<AuditLog>, Pageable> tuple = fetchData(request, AuditLog.class);
//            Page<AuditLog> auditLogPage = auditLogDao.findAll(tuple.getLeft(), tuple.getRight());
//            log.info("Audit log list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            var triplet = fetchList(commonRequestModel);
            return ResponseHelper.buildListSuccessResponse(
                    triplet.getLeft(),
                    triplet.getMiddle(),
                    triplet.getRight());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Triple<List<IRunnerResponse>, Integer, Long> fetchList(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for audit log list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Pair<Specification<AuditLog>, Pageable> tuple = fetchData(request, AuditLog.class);
        Page<AuditLog> auditLogPage = auditLogDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Audit log list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        return Triple.of(
                convertEntityListToDtoList(auditLogPage.getContent()),
                auditLogPage.getTotalPages(),
                auditLogPage.getTotalElements()
        );
    }

    private Long getEntityId(BaseEntity entity) throws NoSuchFieldException, IllegalAccessException {
        Field f = entity.getClass().getSuperclass().getSuperclass().getDeclaredField("id");
        f.setAccessible(true);

        return (Long) f.get(entity);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<AuditLog> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }

    private AuditLogResponse convertEntityToDto(AuditLog auditLog) {
        return jsonHelper.convertValue(auditLog, AuditLogResponse.class);
    }

    private void validateRequest(AuditLogMetaData auditLogMetaData) {
        if (ObjectUtils.isEmpty(auditLogMetaData.getParent()) || ObjectUtils.isEmpty(auditLogMetaData.getParentId())) {
            throw new RunnerException("Parent or parent id is missing");
        } else if (ObjectUtils.isEmpty(auditLogMetaData.getNewData()) && ObjectUtils.isEmpty(auditLogMetaData.getPrevData())) {
            throw new RunnerException("Data is missing for ops " + auditLogMetaData.getOperation());
        }
    }

    private Map<String, AuditLogChanges> getChanges(BaseEntity newEntity, BaseEntity prevEntity, String operation) throws JsonProcessingException, IllegalAccessException {
        List<Field> fields = null;
        if (operation.equals(DBOperationType.CREATE.name())) {
            fields = getListOfAllFields(newEntity);
        } else {
            fields = getListOfAllFields(prevEntity);
        }
        Map<String, AuditLogChanges> fieldValueMap = new HashMap<>();
        fields = fields.stream().filter(field -> !filterFieldBasedOnAnnotation(field)).collect(Collectors.toList());
        for (Field field : fields) {
            String fieldName = field.getName();
            Annotation[] fieldAnnotations = field.getDeclaredAnnotations();

            Object newValue, prevValue;
            AuditLogChanges auditLogChanges = null;


            if (operation.equals(DBOperationType.CREATE.name())) {
                Object temp = field.get(newEntity);
                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(temp)) {
                    newValue = temp.toString();
                } else {
                    newValue = temp;
                }
                auditLogChanges = createAuditLogChangesObject(fieldName, newValue, null);

            } else if (operation.equals(DBOperationType.UPDATE.name())) {
                Object temp = field.get(newEntity);
                Object prevTemp = field.get(prevEntity);
                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(temp)) {
                    newValue = temp.toString();
                } else {
                    newValue = temp;
                }

                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(prevTemp)) {
                    prevValue = prevTemp.toString();
                } else {
                    prevValue = prevTemp;
                }
                if (field.getType() == BigDecimal.class) {
                    BigDecimal number1 = (BigDecimal) newValue;
                    BigDecimal number2 = (BigDecimal) prevValue;
                    if((number1 == null) || (number1 != null && number1.compareTo(number2) != 0)) {
                        if (number1 != null && number2 != null && number1.setScale(5, BigDecimal.ROUND_DOWN).compareTo(number2.setScale(5, BigDecimal.ROUND_DOWN)) != 0) {
                            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
                        } else if (!(number1 == null && number2 == null)) {
                            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
                        }
                    }
                } else if (!ObjectUtils.equals(newValue, prevValue))
                    auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
                else continue;

            } else if (operation.equals(DBOperationType.DELETE.name())) {
                Object prevTemp = field.get(prevEntity);
                if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(prevTemp)) {
                    prevValue = prevTemp.toString();
                } else {
                    prevValue = prevTemp;
                }
                auditLogChanges = createAuditLogChangesObject(fieldName, null, prevValue);
            }

            fieldValueMap.put(fieldName, auditLogChanges);
        }

        ObjectMapper objectMapper = new ObjectMapper();

        return fieldValueMap;
        // return fieldValueMap.toString();
    }

    private List<Field> getListOfAllFields(BaseEntity newEntity) {
        List<Field> privateFields = new ArrayList<>();
        Field[] allFields = newEntity.getClass().getDeclaredFields();
        for (Field field : allFields) {
            if (Modifier.isPrivate(field.getModifiers())) {
                privateFields.add(field);
                field.setAccessible(true);
            }
        }
        return privateFields;
    }

    private boolean filterFieldBasedOnAnnotation(Field field) {
        return Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotationClassList.contains(annotation.annotationType()));
    }

    private AuditLogChanges createAuditLogChangesObject(String fieldName, Object newValue, Object oldValue) {
        return AuditLogChanges.builder()
                .fieldName(fieldName)
                .newValue(newValue)
                .oldValue(oldValue)
                .build();
    }
}
