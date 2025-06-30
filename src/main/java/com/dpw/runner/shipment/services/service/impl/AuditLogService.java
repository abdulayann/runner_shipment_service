package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

import com.dpw.runner.shipment.services.commons.constants.AuditLogConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAuditLogDao;
import com.dpw.runner.shipment.services.dto.response.AuditLogResponse;
import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.ExcelUtils;
import com.dpw.runner.shipment.services.utils.ExcludeAuditLog;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.nimbusds.jose.util.Pair;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.jetbrains.annotations.NotNull;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@SuppressWarnings({"java:S1444", "java:S3008"})
public class AuditLogService implements IAuditLogService {
    private static final Set<Class<?>> annotationClassList = new HashSet<>(Arrays.asList(Id.class, OneToMany.class, ManyToOne.class, ManyToMany.class, ExcludeAuditLog.class));

    public static Map<String, String> COLUMN_HEADERS_TO_FIELD_NAME = null;

    private final Set<DBOperationType> operationTypeEnumSet = EnumSet.of(
        DBOperationType.LOG,
        DBOperationType.DG_REQUEST,
        DBOperationType.DG_APPROVE,
        DBOperationType.DG_REJECT,
        DBOperationType.COMMERCIAL_REQUEST,
        DBOperationType.COMMERCIAL_APPROVE,
        DBOperationType.COMMERCIAL_REJECT
    );

    static {
        COLUMN_HEADERS_TO_FIELD_NAME = new LinkedHashMap<>();
        COLUMN_HEADERS_TO_FIELD_NAME.put("Module", "moduleTypeCode");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Module id", "moduleId");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Field Name", "fieldName");
        COLUMN_HEADERS_TO_FIELD_NAME.put("Old value", AuditLogConstants.OLD_VALUE);
        COLUMN_HEADERS_TO_FIELD_NAME.put("New value", AuditLogConstants.NEW_VALUE);
        COLUMN_HEADERS_TO_FIELD_NAME.put("User Name", Constants.CREATED_BY);
        COLUMN_HEADERS_TO_FIELD_NAME.put("Changed Date", Constants.CREATED_AT);
    }

    @Autowired
    private IAuditLogDao auditLogDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ExcelUtils excelUtils;
    @Autowired
    private CommonUtils commonUtils;

    public Resource downloadExcel(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            var triplet = fetchList(commonRequestModel, null);
            List<Map<String, Object>> listAsMap = getData(triplet.getLeft());
            return excelUtils.createExcelAsResource(listAsMap, COLUMN_HEADERS_TO_FIELD_NAME, "Audit_Logs");
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public List<Map<String, Object>> getData(List<IRunnerResponse> auditLogResponses) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (IRunnerResponse item : auditLogResponses) {
            AuditLogResponse ct = (AuditLogResponse) item;
            Map<String, Object> asMap = new HashMap<>();
            asMap.put("moduleTypeCode", ct.getEntity());
            asMap.put("moduleId", ct.getEntityId().toString());
            asMap.put(Constants.CREATED_BY, ct.getCreatedBy());
            asMap.put(Constants.CREATED_AT, DateUtils.getDateAsString(ct.getCreatedAt()));
            //field name, old value, new value
            JsonNode nodeChanges = jsonHelper.convertValue(ct.getChanges(), JsonNode.class);
            Iterator<String> fieldNames = nodeChanges.fieldNames();
            while (fieldNames.hasNext()) {
                Map<String, Object> clone = new HashMap<>(asMap);
                String field = fieldNames.next();
                clone.put("fieldName", field);
                clone.put(AuditLogConstants.OLD_VALUE, nodeChanges.get(field).get(AuditLogConstants.OLD_VALUE).asText());
                clone.put(AuditLogConstants.NEW_VALUE, nodeChanges.get(field).get(AuditLogConstants.NEW_VALUE).asText());
                result.add(clone);
            }
        }
        return result;
    }

    @Async
    @Transactional
    public void addAuditLog(AuditLogMetaData auditLogMetaData) throws IllegalAccessException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, NoSuchMethodException, RunnerException {
        String skipAuditLog = MDC.get("skip-audit-log");
        if(skipAuditLog != null && skipAuditLog.equals("true"))
            return;
        validateRequest(auditLogMetaData);
        AuditLog auditLog = new AuditLog();
        auditLog.setOperation(auditLogMetaData.getOperation());
        auditLog.setParentType(auditLogMetaData.getParent());
        auditLog.setParentId(auditLogMetaData.getParentId());
        auditLog.setTenantId(auditLogMetaData.getTenantId());
        auditLog.setCreatedBy(auditLogMetaData.getUserName());
        auditLog.setUpdatedBy(auditLogMetaData.getUserName());
        auditLog.setCreatedAt(LocalDateTime.now());
        auditLog.setUpdatedAt(LocalDateTime.now());
        auditLog.setIsIntegrationLog(auditLogMetaData.getIsIntegrationLog());
        auditLog.setFlow(auditLogMetaData.getFlow());
        auditLog.setDataType(auditLogMetaData.getDataType());
        String ops = auditLogMetaData.getOperation();

        if (ops.equals(DBOperationType.CREATE.name())) {
            prepareAuditLogOfCreate(auditLogMetaData, auditLog, ops);
        } else if (ops.equals(DBOperationType.UPDATE.name())) {
            prepareAuditLogOfUpdate(auditLogMetaData, auditLog, ops);
        } else if (ops.equals(DBOperationType.DELETE.name())) {
            prepareAuditLogOfDelete(auditLogMetaData, auditLog, ops);
        } else if (operationTypeEnumSet.contains(DBOperationType.valueOf(ops))) {
            prepareAuditLogOfLog(auditLogMetaData, auditLog, ops);
        } else {
            throw new RunnerException("Not a valid operation performed");
        }
        if(ops.equals(DBOperationType.UPDATE.name()) && (auditLog.getChanges() == null || auditLog.getChanges().size() == 0))
            return;
        auditLogDao.save(auditLog);
    }

    private void prepareAuditLogOfLog(AuditLogMetaData auditLogMetaData, AuditLog auditLog, String ops)
            throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        Map<String, AuditLogChanges> auditLogChangesMap = getChanges(auditLogMetaData.getNewData(), null, ops);
        if (!auditLogChangesMap.isEmpty()) {
            addBaseEntityFields(auditLogChangesMap, null, auditLogMetaData.getNewData(), auditLogMetaData.getUserName());
        }
        auditLog.setChanges(auditLogChangesMap);
        auditLog.setEntity(auditLogMetaData.getEntityType());
    }

    private void prepareAuditLogOfDelete(AuditLogMetaData auditLogMetaData, AuditLog auditLog, String ops)
            throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException, NoSuchFieldException {
        Map<String, AuditLogChanges> auditLogChangesMap = getChanges(null, auditLogMetaData.getPrevData(), ops);
        if(!auditLogChangesMap.isEmpty())
            addBaseEntityFields(auditLogChangesMap, auditLogMetaData.getPrevData(), null, auditLogMetaData.getUserName());
        auditLog.setChanges(auditLogChangesMap);
        auditLog.setEntity(auditLogMetaData.getPrevData().getClass().getSimpleName());
        auditLog.setEntityId(getEntityId(auditLogMetaData.getPrevData()));
    }

    private void prepareAuditLogOfUpdate(AuditLogMetaData auditLogMetaData, AuditLog auditLog, String ops)
            throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException, NoSuchFieldException {
        Map<String, AuditLogChanges> auditLogChangesMap = getChanges(auditLogMetaData.getNewData(), auditLogMetaData.getPrevData(), ops);
        if(!auditLogChangesMap.isEmpty())
            addBaseEntityFields(auditLogChangesMap, auditLogMetaData.getPrevData(), auditLogMetaData.getNewData(), auditLogMetaData.getUserName());
        auditLog.setChanges(auditLogChangesMap);
        auditLog.setEntity(auditLogMetaData.getNewData().getClass().getSimpleName());
        auditLog.setEntityId(getEntityId(auditLogMetaData.getNewData()));
    }

    private void prepareAuditLogOfCreate(AuditLogMetaData auditLogMetaData, AuditLog auditLog, String ops)
            throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException, NoSuchFieldException {
        Map<String, AuditLogChanges> auditLogChangesMap = getChanges(auditLogMetaData.getNewData(), null, ops);
        if(!auditLogChangesMap.isEmpty())
            addBaseEntityFields(auditLogChangesMap, null, auditLogMetaData.getNewData(), auditLogMetaData.getUserName());
        auditLog.setChanges(auditLogChangesMap);
        auditLog.setEntity(auditLogMetaData.getNewData().getClass().getSimpleName());
        auditLog.setEntityId(getEntityId(auditLogMetaData.getNewData()));
    }

    public void addBaseEntityFields(Map<String, AuditLogChanges> auditLogChangesMap, BaseEntity oldEntity, BaseEntity newEntity, String userName)
    {
        if(newEntity == null)
            return;
        if(oldEntity == null)
        {
            auditLogChangesMap.put(Constants.CREATED_AT, createAuditLogChangesObject(Constants.CREATED_AT, LocalDateTime.now().toString(), null));
            auditLogChangesMap.put(Constants.CREATED_BY, createAuditLogChangesObject(Constants.CREATED_BY, userName, null));
        }
        else
        {
            auditLogChangesMap.put("updatedAt", createAuditLogChangesObject("updatedAt", LocalDateTime.now().toString(), oldEntity.getUpdatedAt() != null ? oldEntity.getUpdatedAt().toString() : null));
            if(!Objects.equals(userName, oldEntity.getUpdatedBy()))
                auditLogChangesMap.put("updatedBy", createAuditLogChangesObject("updatedBy", userName, oldEntity.getUpdatedBy()));
        }
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, String xSource) {
        String responseMsg;
        try {
            var triplet = fetchList(commonRequestModel, xSource);
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

    public Triple<List<IRunnerResponse>, Integer, Long> fetchList(CommonRequestModel commonRequestModel, String xSource) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for audit log list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Pair<Specification<AuditLog>, Pageable> tuple = fetchData(request, AuditLog.class);
        Page<AuditLog> auditLogPage;
        if(Objects.equals(xSource, Constants.NETWORK_TRANSFER))
            auditLogPage = auditLogDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
        else
            auditLogPage = auditLogDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Audit log list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        return Triple.of(
                convertEntityListToDtoList(auditLogPage.getContent()),
                auditLogPage.getTotalPages(),
                auditLogPage.getTotalElements()
        );
    }

    public Long getEntityId(BaseEntity entity) throws NoSuchFieldException, IllegalAccessException {
        if(Objects.isNull(entity.getClass().getSuperclass().getSuperclass())) {
            return null;
        }
        Field f = entity.getClass().getSuperclass().getSuperclass().getDeclaredField("id");
        f.setAccessible(true);

        return (Long) f.get(entity);
    }

    public List<IRunnerResponse> convertEntityListToDtoList(final List<AuditLog> list) {
        return list.stream()
                .map(this::convertEntityToDto)
                .collect(Collectors.toList());
    }

    public AuditLogResponse convertEntityToDto(AuditLog auditLog) {
        AuditLogResponse response = new AuditLogResponse();
        response.setId(auditLog.getId());
        response.setOperation(auditLog.getOperation());
        response.setEntityId(auditLog.getEntityId());
        response.setEntity(auditLog.getEntity());
        response.setParentId(auditLog.getParentId());
        response.setParentType(auditLog.getParentType());
        response.setCreatedAt(auditLog.getCreatedAt());
        response.setCreatedBy(auditLog.getCreatedBy());
        response.setFlow(auditLog.getFlow());
        response.setDataType(auditLog.getDataType());

        List<AuditLogChanges> changes = new ArrayList<>();
        for (Map.Entry<String, AuditLogChanges> entry : auditLog.getChanges().entrySet()) {
            AuditLogChanges auditChange = entry.getValue();
            auditChange.setFieldName(replaceFieldNames(auditLog, entry.getKey()));
            changes.add(entry.getValue());
        }

        response.setChanges(changes);
        return response;
    }

    public String replaceFieldNames(AuditLog auditLog, String key) {
        if (!Objects.equals(auditLog.getParentType(), ShipmentDetails.class.getSimpleName())) {
            return key;
        }

        Map<String, Map<String, String>> entityToFieldMap = new HashMap<>();
        entityToFieldMap.put(ShipmentDetails.class.getSimpleName(), AuditLogConstants.ShipmentsFieldNameToDisplayNameMap);
        entityToFieldMap.put(Containers.class.getSimpleName(), AuditLogConstants.ContainerFieldNameToDisplayNameMap);
        entityToFieldMap.put(Packing.class.getSimpleName(), AuditLogConstants.PackingFieldNameToDisplayNameMap);
        entityToFieldMap.put(Routings.class.getSimpleName(), AuditLogConstants.RoutingsFieldNameToDisplayNameMap);
        entityToFieldMap.put(Notes.class.getSimpleName(), AuditLogConstants.NotesFieldNameToDisplayNameMap);
        entityToFieldMap.put(BookingCarriage.class.getSimpleName(), AuditLogConstants.BookingCarriageFieldNameToDisplayNameMap);
        entityToFieldMap.put(Events.class.getSimpleName(), AuditLogConstants.EventsFieldNameToDisplayNameMap);
        entityToFieldMap.put(ReferenceNumbers.class.getSimpleName(), AuditLogConstants.ReferenceNumbersFieldNameToDisplayNameMap);
        entityToFieldMap.put(Parties.class.getSimpleName(), AuditLogConstants.PartiesFieldNameToDisplayNameMap);
        entityToFieldMap.put(ServiceDetails.class.getSimpleName(), AuditLogConstants.ServiceDetailsFieldNameToDisplayNameMap);
        entityToFieldMap.put(TruckDriverDetails.class.getSimpleName(), AuditLogConstants.TruckDriverDetailsFieldNameToDisplayName);

        Map<String, String> fieldMap = entityToFieldMap.get(auditLog.getEntity());
        if (fieldMap != null && fieldMap.containsKey(key)) {
            return fieldMap.get(key);
        }

        return key;
    }

    private void validateRequest(@NotNull AuditLogMetaData auditLogMetaData) throws RunnerException {
        if (ObjectUtils.isEmpty(auditLogMetaData.getParent()) || ObjectUtils.isEmpty(auditLogMetaData.getParentId())) {
            throw new RunnerException("Parent or parent id is missing");
        } else if (ObjectUtils.isEmpty(auditLogMetaData.getNewData()) && ObjectUtils.isEmpty(auditLogMetaData.getPrevData())) {
            throw new RunnerException("Data is missing for ops " + auditLogMetaData.getOperation());
        }
    }

    public Map<String, AuditLogChanges> getChanges(BaseEntity newEntity, BaseEntity prevEntity, String operation) throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        List<Field> fields = null;
        if(newEntity == null && prevEntity == null)
            return new HashMap<>();
        if (operation.equals(DBOperationType.CREATE.name()) || operationTypeEnumSet.contains(DBOperationType.valueOf(operation))) {
            fields = getListOfAllFields(newEntity);
        } else {
            fields = getListOfAllFields(prevEntity);
        }
        Map<String, AuditLogChanges> fieldValueMap = new HashMap<>();
        fields = fields.stream().filter(field -> !filterFieldBasedOnAnnotation(field)).toList();
        for (Field field : fields) {

            String fieldName = field.getName();

            if(fieldName.startsWith("cachedValue$") || fieldName.startsWith("$$_hibernate_interceptor" ) || fieldName.equalsIgnoreCase("serialVersionUID")) continue;

            AuditLogChanges auditLogChanges = null;

            auditLogChanges = getAuditLogChangesForOperation(newEntity, prevEntity, operation, field, auditLogChanges, fieldName, fieldValueMap);
            if(auditLogChanges != null)
                fieldValueMap.put(fieldName, auditLogChanges);
        }

        return fieldValueMap;
    }

    private AuditLogChanges getAuditLogChangesForOperation(BaseEntity newEntity, BaseEntity prevEntity, String operation, Field field, AuditLogChanges auditLogChanges, String fieldName, Map<String, AuditLogChanges> fieldValueMap) throws IllegalAccessException, InvocationTargetException, JsonProcessingException, NoSuchMethodException {
        if (operation.equals(DBOperationType.CREATE.name())) {
            auditLogChanges = getAuditLogChangesForCreateOperation(newEntity, field, fieldName, fieldValueMap, auditLogChanges);
        } else if (operationTypeEnumSet.contains(DBOperationType.valueOf(operation))) {
            auditLogChanges = getAuditLogChangesForOperationTypeEnumSet(newEntity, field, fieldName, auditLogChanges);
        } else if (operation.equals(DBOperationType.UPDATE.name())) {
            auditLogChanges = getAuditLogChangesForUpdateOperation(newEntity, prevEntity, field, fieldName, auditLogChanges, fieldValueMap);
        }
        else if (operation.equals(DBOperationType.DELETE.name())) {
            auditLogChanges = getAuditLogChangesForDeleteOperation(prevEntity, field, fieldName, fieldValueMap, auditLogChanges);
        }
        return auditLogChanges;
    }

    private AuditLogChanges getAuditLogChangesForUpdateOperation(BaseEntity newEntity, BaseEntity prevEntity, Field field, String fieldName, AuditLogChanges auditLogChanges, Map<String, AuditLogChanges> fieldValueMap) throws IllegalAccessException, InvocationTargetException, JsonProcessingException, NoSuchMethodException {
        Object prevValue;
        Object newValue;
        Object temp = field.get(newEntity);
        Object prevTemp = field.get(prevEntity);
        try{
            temp  = PropertyUtils.getProperty(newEntity, fieldName);
            prevTemp = PropertyUtils.getProperty(prevEntity, fieldName);
        } catch(NoSuchMethodException e){
            log.error(e.getMessage());
        }

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
            auditLogChanges = getAuditLogChangesForBigDecimalClass(newValue, prevValue, auditLogChanges, fieldName);
        }
        else if(Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotation.annotationType() == OneToOne.class))
        {
            processOneToOneClassOperation(temp, prevTemp, fieldName, fieldValueMap);
        }
        else if (!Objects.equals(newValue, prevValue)){
            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
        }
        return auditLogChanges;
    }

    private AuditLogChanges getAuditLogChangesForOperationTypeEnumSet(BaseEntity newEntity, Field field, String fieldName, AuditLogChanges auditLogChanges) throws IllegalAccessException, InvocationTargetException {
        Object newValue;
        Object temp = field.get(newEntity);
        try {
            temp = PropertyUtils.getProperty(newEntity, fieldName);
        } catch (NoSuchMethodException e) {
            log.error(e.getMessage());
        }
        if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(temp)) {
            newValue = temp.toString();
        } else {
            newValue = temp;
        }
        if (newValue != null) {
            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, null);
        }
        return auditLogChanges;
    }

    private AuditLogChanges getAuditLogChangesForBigDecimalClass(Object newValue, Object prevValue, AuditLogChanges auditLogChanges, String fieldName) {
        BigDecimal number1 = (BigDecimal) newValue;
        BigDecimal number2 = (BigDecimal) prevValue;
        if(areBigDecimalsDifferent(number1, number2)) {
            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, prevValue);
        }
        return auditLogChanges;
    }

    public boolean areBigDecimalsDifferent(BigDecimal number1, BigDecimal number2) {
        if (number1 == null && number2 == null) {
            return false; // Both null, consider them equal
        }
        if (number1 == null || number2 == null) {
            return true; // one is null, the other isn't, they are different.
        }

        return CommonUtils.roundBigDecimal(number1, 5, RoundingMode.DOWN)
                .compareTo(CommonUtils.roundBigDecimal(number2, 5, RoundingMode.DOWN)) != 0;
    }

    private AuditLogChanges getAuditLogChangesForCreateOperation(BaseEntity newEntity, Field field, String fieldName, Map<String, AuditLogChanges> fieldValueMap, AuditLogChanges auditLogChanges) throws IllegalAccessException, InvocationTargetException, JsonProcessingException, NoSuchMethodException {
        Object newValue;
        Object temp = field.get(newEntity);
        try{
            temp  = PropertyUtils.getProperty(newEntity, fieldName);
        } catch(NoSuchMethodException e) {
            log.info(Constants.IGNORED_ERROR_MSG);
        }
        if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(temp)) {
            newValue = temp.toString();
        } else {
            newValue = temp;
        }
        if(Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotation.annotationType() == OneToOne.class))
        {
            // Handle related entities (one-to-one or one-to-many relationships)
            Map<String, AuditLogChanges> childChanges = getChanges((BaseEntity) temp, null, DBOperationType.CREATE.name());

            // Prefix the property names with the entity name
            for (Map.Entry<String, AuditLogChanges> entry : childChanges.entrySet()) {
                String propertyName = fieldName + "." + entry.getKey();
                fieldValueMap.put(propertyName, entry.getValue());
            }
        }
        else if(newValue != null)
            auditLogChanges = createAuditLogChangesObject(fieldName, newValue, null);
        return auditLogChanges;
    }

    private void processOneToOneClassOperation(Object temp, Object prevTemp, String fieldName, Map<String, AuditLogChanges> fieldValueMap) throws JsonProcessingException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        var op = DBOperationType.UPDATE.name();
        if(temp == null)
            op = DBOperationType.DELETE.name();
        else if(prevTemp == null)
            op = DBOperationType.CREATE.name();
        // Handle related entities (one-to-one or one-to-many relationships)
        Map<String, AuditLogChanges> childChanges = getChanges((BaseEntity) temp, (BaseEntity) prevTemp, op);

        // Prefix the property names with the entity name
        for (Map.Entry<String, AuditLogChanges> entry : childChanges.entrySet()) {
            String propertyName = fieldName + "." + entry.getKey();
            fieldValueMap.put(propertyName, entry.getValue());
        }
    }

    private AuditLogChanges getAuditLogChangesForDeleteOperation(BaseEntity prevEntity, Field field, String fieldName, Map<String, AuditLogChanges> fieldValueMap, AuditLogChanges auditLogChanges) throws IllegalAccessException, JsonProcessingException, InvocationTargetException, NoSuchMethodException {
        Object prevValue;
        Object prevTemp = field.get(prevEntity);
        if (field.getType() == LocalDateTime.class && !ObjectUtils.isEmpty(prevTemp)) {
            prevValue = prevTemp.toString();
        } else {
            prevValue = prevTemp;
        }
        if(Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotation.annotationType() == OneToOne.class))
        {
            Map<String, AuditLogChanges> childChanges = getChanges(null, (BaseEntity) prevTemp, DBOperationType.DELETE.name());

            // Prefix the property names with the entity name
            for (Map.Entry<String, AuditLogChanges> entry : childChanges.entrySet()) {
                String propertyName = fieldName + "." + entry.getKey();
                fieldValueMap.put(propertyName, entry.getValue());
            }
        }
        else if(prevValue != null)
            auditLogChanges = createAuditLogChangesObject(fieldName, null, prevValue);
        return auditLogChanges;
    }

    public List<Field> getListOfAllFields(BaseEntity newEntity) {
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

    public boolean filterFieldBasedOnAnnotation(Field field) {
        return Arrays.stream(field.getDeclaredAnnotations()).anyMatch(annotation -> annotationClassList.contains(annotation.annotationType()));
    }

    public AuditLogChanges createAuditLogChangesObject(String fieldName, Object newValue, Object oldValue) {
        return AuditLogChanges.builder()
                .fieldName(fieldName)
                .newValue(newValue)
                .oldValue(oldValue)
                .build();
    }

    public static <T extends Comparable<T>> int compareTo(final T c1, final T c2) {
        if (c1 == null && c2 == null) {
            return 0;
        }
        if (c1 == null) {
            return -1;
        }
        if (c2 == null) {
            return 1;
        }
        return c1.compareTo(c2);
    }
}
