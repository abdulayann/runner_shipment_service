package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.ObjectUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.lang.reflect.Field;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import static org.springframework.data.jpa.domain.Specification.where;

@SuppressWarnings("ALL")
@Slf4j
public class DbAccessHelper {
    private DbAccessHelper(){}
    public static final String YYYY_MM_DD = "yyyy-MM-dd";

    public static <T> Pair<Specification<T>, Pageable> fetchData(ListCommonRequest request, Class className, Map<String, RunnerEntityMapping> tableNames) {
        log.info("RequestId {}, Received Criteria Request from {}", LoggerHelper.getRequestIdFromMDC(), className.getSimpleName());
        Pageable pages;
        globalSearchCriteria(request, tableNames);
        if (request.getSortRequest() != null && request.getFilterCriteria() != null &&
                (request.getFilterCriteria().size() == 0  || (request.getFilterCriteria().size() == 1 && request.getFilterCriteria().get(0).getInnerFilter() != null && request.getFilterCriteria().get(0).getInnerFilter().size() == 0))) {
            String _tableName = !Objects.isNull(tableNames.get(request.getSortRequest().getFieldName())) ? tableNames.get(request.getSortRequest().getFieldName()).getTableName() : null;
            Sort sortRequest = null;
            if (_tableName != null) {
                if (Objects.equals(_tableName, className.getSimpleName()))
                    sortRequest = Sort.by(getFieldName(request.getSortRequest().getFieldName(), tableNames));
                else
                    sortRequest = Sort.by( _tableName + "." + getFieldName(request.getSortRequest().getFieldName(), tableNames));
                sortRequest = sortRequest.ascending();
                if (Objects.equals(request.getSortRequest().getOrder(), "DESC"))
                    sortRequest = sortRequest.descending();
            }
            pages = PageRequest.of(request.getPageNo() - 1, request.getPageSize(), sortRequest);
        } else {
            pages = PageRequest.of(request.getPageNo() - 1, request.getPageSize());
        }
        List<FilterCriteria> filterCriteria = (request.getFilterCriteria() == null ? new ArrayList<FilterCriteria>() : request.getFilterCriteria());
        SortRequest sortRequest = request.getSortRequest();

        Specification<T> specification = null;
        Map<String, Join<Class, T>> map = new HashMap<>();
        if(filterCriteria.size() == 0) {
            specification = where(createSpecificationWithoutFilter(request.getIncludeTbls()));
        }
        for (FilterCriteria filters : filterCriteria) {
            if (filters.getLogicOperator() == null) {
                specification =
                        where(getSpecificationFromFilters(filters.getInnerFilter(), sortRequest, map, className.getSimpleName(), request.getIncludeTbls(), tableNames));
            } else if (filters.getLogicOperator().equalsIgnoreCase("OR")) {
                specification = specification.or(getSpecificationFromFilters(filters.getInnerFilter(), null, map, className.getSimpleName(), null, tableNames));
            } else if (filters.getLogicOperator().equalsIgnoreCase("AND")) {
                specification = specification.and(getSpecificationFromFilters(filters.getInnerFilter(), null, map, className.getSimpleName(), null, tableNames));
            }
        }
        log.info("RequestId {}, Received Criteria Request from {} got completed", LoggerHelper.getRequestIdFromMDC(), className.getSimpleName());
        map.clear();
        return Pair.of(specification, pages);
    }

    private static void globalSearchCriteria(ListCommonRequest request, Map<String, RunnerEntityMapping> tableName) {
        if (StringUtility.isEmpty(request.getContainsText()))
            return;
        List<FilterCriteria> criterias = createCriteriaForGlobalSearch(tableName, request.getContainsText());
        FilterCriteria criteria1 = FilterCriteria.builder().innerFilter(request.getFilterCriteria()).build();
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).build();
        if(criteria1 != null && criteria1.getInnerFilter().size() > 0){
            criteria2.setLogicOperator("AND");
            request.setFilterCriteria(Arrays.asList(criteria1, criteria2));
        }
        else
            request.setFilterCriteria(Arrays.asList(criteria2));
    }

    private static List<FilterCriteria> createCriteriaForGlobalSearch(Map<String, RunnerEntityMapping> tableName, String containsText) {
        List<FilterCriteria> innerFilters = new ArrayList<>();
        for (String key: tableName.keySet()) {
            if(tableName.get(key).isContainsText()) {
                innerFilters.add(FilterCriteria.builder().logicOperator(innerFilters.isEmpty() ? null : "OR")
                        .criteria(Criteria.builder().fieldName(key).value(containsText).operator("LIKE").build()).build());
            }
        }
//        List<RunnerEntityMapping> entityMappingList = tableName.entrySet().stream()
//                        .filter(x -> x.getValue().isContainsText()).map(x -> x.getValue()).toList();
//        entityMappingList.forEach(c -> {
//
//
//        });
        return innerFilters.isEmpty() ? null : innerFilters;
    }


    public static <T> Pair<Specification<T>, Pageable> fetchData(ListCommonRequest request, Class className) {
        Pageable pages;
        if (request.getSortRequest() != null && request.getFilterCriteria() != null && (request.getFilterCriteria().size() == 0  || (request.getFilterCriteria().size() == 1 && request.getFilterCriteria().get(0).getInnerFilter() != null))) {
            Sort sortRequest = Sort.by(request.getSortRequest().getFieldName());
            sortRequest = sortRequest.ascending();
            if (Objects.equals(request.getSortRequest().getOrder(), "DESC"))
                sortRequest = sortRequest.descending();
            pages = PageRequest.of(request.getPageNo() - 1, request.getPageSize(), sortRequest);
        } else {
            pages = PageRequest.of(request.getPageNo() - 1, request.getPageSize());
        }
        List<FilterCriteria> filterCriteria = (request.getFilterCriteria() == null ? new ArrayList<FilterCriteria>() : request.getFilterCriteria());
        SortRequest sortRequest = request.getSortRequest();

        Map<String, Class> dataTypeMap = new HashMap<>();
        ObjectUtility.getAllFields(className, dataTypeMap);

        Specification<T> specification = null;
        Map<String, Join<Class, T>> map = new HashMap<>();
        for (FilterCriteria filters : filterCriteria) {
            if (filters.getLogicOperator() == null) {
                specification =
                        where(getSpecificationFromFiltersWithoutMapping(filters.getInnerFilter(), sortRequest, map, className.getSimpleName(), dataTypeMap));
            } else if (filters.getLogicOperator().equalsIgnoreCase("OR")) {
                specification = specification.or(getSpecificationFromFiltersWithoutMapping(filters.getInnerFilter(), null, map, className.getSimpleName(), dataTypeMap));
            } else if (filters.getLogicOperator().equalsIgnoreCase("AND")) {
                specification = specification.and(getSpecificationFromFiltersWithoutMapping(filters.getInnerFilter(), null, map, className.getSimpleName(), dataTypeMap));
            }
        }
        return Pair.of(specification, pages);
    }

    private static <T> Specification<T> getSpecificationFromFilters(List<FilterCriteria> filter, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className, List<String> tableName, Map<String, RunnerEntityMapping> tableNames) {
        if (filter == null || filter.size() == 0) {
            return createSpecificationWithoutFilter(tableName);
        }

        Specification<T> specification = null;

        for (FilterCriteria input : filter) {
            if (input.getInnerFilter() != null && input.getInnerFilter().size() > 0) {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(getSpecificationFromFilters(input.getInnerFilter(), null, map, className, null, tableNames));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(getSpecificationFromFilters(input.getInnerFilter(), null, map, className, null, tableNames));
                    }
                } else {
                    specification =
                            where(getSpecificationFromFilters(input.getInnerFilter(), sortRequest, map, className, tableName, tableNames));
                }
            } else {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(createSpecification(input.getCriteria(), null, map, className, null, tableNames));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(createSpecification(input.getCriteria(), null, map, className, null, tableNames));
                    }
                } else {
                    specification =
                            where(createSpecification(input.getCriteria(), sortRequest, map, className, tableName, tableNames));
                }
            }
        }
        return specification;
    }

    private static <T> Specification<T> createSpecificationWithoutFilter(List<String> tableName) {
        if (tableName != null) {
            return (root, query, criteriaBuilder) -> {
                if (!query.getResultType().isAssignableFrom(Long.class)) {
                    for (String table : tableName) {
                        Join<Class, T> join = (Join) root.fetch(table, JoinType.LEFT);
                        //query.distinct(true);
                    }
                }
                return criteriaBuilder.conjunction();
            };
        }
        return null;
    }

    private static <T> Specification<T> createSpecification(Criteria input, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className, List<String> tableName, Map<String, RunnerEntityMapping> tableNames) {
        return (root, query, criteriaBuilder) -> {
            Path path = null;
            Join<Class, T> join;
            if(!query.getResultType().isAssignableFrom(Long.class) && tableName != null) {
                for (String table : tableName) {
                    join = (Join) root.fetch(table, JoinType.LEFT);
                    map.put(table, join);
                    query.distinct(true);
                }
            }

            if (!query.getResultType().isAssignableFrom(Long.class) && sortRequest != null && (query.getOrderList() == null || query.getOrderList().size() == 0)) {
                if (tableNames.get(sortRequest.getFieldName()).getTableName().equalsIgnoreCase(className)) {
                    if (sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                        query.orderBy(Arrays.asList(criteriaBuilder.desc(root.get(getFieldName(sortRequest.getFieldName(), tableNames)))));
                    } else {
                        query.orderBy(Arrays.asList(criteriaBuilder.asc(root.get(getFieldName(sortRequest.getFieldName(), tableNames)))));
                    }
                } else {
                    if ((root.getJoins() == null && root.getFetches() == null) || (root.getJoins().size() == 0 && root.getFetches().size() == 0) || map.get(tableNames.get(sortRequest.getFieldName()).getTableName()) == null ||
                            (!root.getJoins().contains(map.get(tableNames.get(sortRequest.getFieldName()).getTableName())) && !root.getFetches().contains(map.get(tableNames.get(sortRequest.getFieldName()).getTableName())))) {
                        join = (Join) root.fetch(tableNames.get(sortRequest.getFieldName()).getTableName(), JoinType.LEFT);
                        map.put(tableNames.get(sortRequest.getFieldName()).getTableName(), join);
                        query.distinct(true);
                    } else {
                        join = map.get(tableNames.get(sortRequest.getFieldName()).getTableName());
                    }
                    if (sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                        query.orderBy(Arrays.asList(criteriaBuilder.desc(((Join) join).get(getFieldName(sortRequest.getFieldName(), tableNames)))));
                    } else {
                        query.orderBy(Arrays.asList(criteriaBuilder.asc(((Join) join).get(getFieldName(sortRequest.getFieldName(), tableNames)))));
                    }
                }
            }

            if (tableNames.get(input.getFieldName()).getTableName().equalsIgnoreCase(className)) {
                path = root;
            } else {
                if ((root.getJoins() == null && root.getFetches() == null) || (root.getJoins().size() == 0 && root.getFetches().size() == 0) || map.get(tableNames.get(input.getFieldName()).getTableName()) == null ||
                        (!root.getJoins().contains(map.get(tableNames.get(input.getFieldName()).getTableName())) && !root.getFetches().contains(map.get(tableNames.get(input.getFieldName()).getTableName())))) {
                    join = root.join(tableNames.get(input.getFieldName()).getTableName(), JoinType.LEFT);
                    map.put(tableNames.get(input.getFieldName()).getTableName(), join);
                    path = join;
                    query.distinct(true);
                } else {
                    path = map.get(tableNames.get(input.getFieldName()).getTableName());
                }
            }
            return createSpecification(tableNames.get(input.getFieldName()).getDataType(), input, path, criteriaBuilder, getFieldName(input.getFieldName(), tableNames));

        };
    }

    private static String getFieldName(String key,  Map<String, RunnerEntityMapping> tableNames) {
        return tableNames.get(key).getFieldName() == null ? key : tableNames.get(key).getFieldName();
    }

    static private Enum<?> getEnum(String enumFullName, String enumName) {
        @SuppressWarnings("unchecked")
        final Class<Enum> cl;
        try {
            cl = (Class<Enum>)Class.forName(enumFullName);
            @SuppressWarnings("unchecked")
            final Enum result = Enum.valueOf(cl, enumName);
            return result;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }

    private static <T> Predicate createSpecification(Class dataType, Criteria input, Path path, CriteriaBuilder criteriaBuilder, String fieldName) {
        switch (input.getOperator()) {
            case "=":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.equal(criteriaBuilder.lower(path.get(fieldName)), (((String) input.getValue()).toLowerCase()));
                }
                else if (dataType.isAssignableFrom(UUID.class) && input.getValue() instanceof String) {
                    return criteriaBuilder.equal(path.get(fieldName), UUID.fromString((String) input.getValue()));
                }
                else if(dataType.isEnum()) {
                    return criteriaBuilder.equal(path.get(fieldName), getEnum(dataType.getName(), (String) input.getValue()));
                }
                return criteriaBuilder.equal(path.get(fieldName), input.getValue());

            case "!=":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.notEqual(criteriaBuilder.lower(path.get(fieldName)), (((String) input.getValue()).toLowerCase()));
                }
                return criteriaBuilder.notEqual(path.get(fieldName), input.getValue());

            case ">":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.greaterThan(path.get(fieldName), (String) input.getValue());
                }
                if (dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.greaterThan(path.get(fieldName), covertStringToData((String) input.getValue(), YYYY_MM_DD));
                }
                if (dataType.isAssignableFrom(LocalDateTime.class)) {
                    if(input.getValue() instanceof LocalDateTime) {
                        return criteriaBuilder.greaterThan(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, (LocalDateTime) input.getValue()));
                    }
                    return criteriaBuilder.greaterThan(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, covertStringToLocalDate((String) input.getValue(), YYYY_MM_DD)));
                }
                return criteriaBuilder.gt(path.get(fieldName), (Number) input.getValue());

            case "<":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.lessThan(path.get(fieldName), (String) input.getValue());
                }
                if (dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.lessThan(path.get(fieldName), covertStringToData((String) input.getValue(), YYYY_MM_DD));
                }
                if (dataType.isAssignableFrom(LocalDateTime.class)) {
                    if(input.getValue() instanceof LocalDateTime) {
                        return criteriaBuilder.lessThan(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, (LocalDateTime) input.getValue()));
                    }
                    return criteriaBuilder.lessThan(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, covertStringToLocalDate((String) input.getValue(), YYYY_MM_DD)));
                }
                return criteriaBuilder.lt(path.get(fieldName), (Number) input.getValue());
            case ">=":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.greaterThanOrEqualTo(path.get(fieldName), (String) input.getValue());
                }
                if (dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.greaterThanOrEqualTo(path.get(fieldName), covertStringToData((String) input.getValue(), YYYY_MM_DD));
                }
                if (dataType.isAssignableFrom(LocalDateTime.class)) {
                    if(input.getValue() instanceof LocalDateTime) {
                        return criteriaBuilder.greaterThanOrEqualTo(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, (LocalDateTime) input.getValue()));
                    }
                    return criteriaBuilder.greaterThanOrEqualTo(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, covertStringToLocalDate((String) input.getValue(), YYYY_MM_DD)));
                }
                return criteriaBuilder.gt(path.get(fieldName), (Number) input.getValue());

            case "<=":
                if (dataType.isAssignableFrom(String.class)) {
                    return criteriaBuilder.lessThanOrEqualTo(path.get(fieldName), (String) input.getValue());
                }
                if (dataType.isAssignableFrom(Date.class)) {
                    return criteriaBuilder.lessThanOrEqualTo(path.get(fieldName), covertStringToData((String) input.getValue(), YYYY_MM_DD));
                }
                if (dataType.isAssignableFrom(LocalDateTime.class)) {
                    if(input.getValue() instanceof LocalDateTime) {
                        return criteriaBuilder.lessThanOrEqualTo(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, (LocalDateTime) input.getValue()));
                    }
                    return criteriaBuilder.lessThanOrEqualTo(path.get(fieldName), convertFromTenantTimeZone(path, fieldName, covertStringToLocalDate((String) input.getValue(), YYYY_MM_DD)));
                }
                return criteriaBuilder.lt(path.get(fieldName), (Number) input.getValue());

            case "LIKE":
                return criteriaBuilder.like(criteriaBuilder.lower(path.get(fieldName)),
                        "%" + ((String) input.getValue()).toLowerCase() + "%");

            case "STARTSWITH":
                return criteriaBuilder.like(criteriaBuilder.lower(path.get(fieldName)),
                        ((String) input.getValue()).toLowerCase() + "%");

            case "IN":
                if (dataType.isAssignableFrom(UUID.class) && input.getValue() != null && input.getValue() instanceof List) {
                    List<UUID> querySet = ((List<?>) input.getValue()).stream()
                            .map(i -> {
                                if (i instanceof String)
                                    return UUID.fromString((String) i);
                                return (UUID) i;
                            }).toList();
                    return criteriaBuilder.in(path.get(fieldName)).value(querySet);
                }
                if (dataType.isAssignableFrom(Long.class) && input.getValue() != null && input.getValue() instanceof List) {
                    List<Long> querySet = ((List<?>) input.getValue()).stream()
                            .map(i -> Long.valueOf(String.valueOf(i))).toList();
                    return criteriaBuilder.in(path.get(fieldName)).value(querySet);
                }
                if (dataType.isEnum()) {
                    List<Enum> querySet = ((List<?>) input.getValue()).stream()
                            .map(i -> getEnum(dataType.getName(), String.valueOf(i))).collect(Collectors.toList());
                    return criteriaBuilder.in(path.get(fieldName)).value(querySet);
                }
                return criteriaBuilder.in(path.get(fieldName))
                        .value(input.getValue());
            case "NOTIN":
                if (dataType.isAssignableFrom(UUID.class) && input.getValue() != null && input.getValue() instanceof List) {
                    List<UUID> querySet = ((List<?>) input.getValue()).stream()
                            .map(i -> {
                                if (i instanceof String string)
                                    return UUID.fromString(string);
                                return (UUID) i;
                            }).toList();
                    return criteriaBuilder.in(path.get(fieldName)).value(querySet).not();
                }
                if (dataType.isAssignableFrom(Long.class) && input.getValue() != null && input.getValue() instanceof List) {
                    List<Long> querySet = ((List<?>) input.getValue()).stream()
                            .map(i -> Long.valueOf(String.valueOf(i))).toList();
                    return criteriaBuilder.in(path.get(fieldName)).value(querySet).not();
                }
                if (dataType.isEnum()) {
                    List<Enum> querySet = ((List<?>) input.getValue()).stream()
                            .map(i -> getEnum(dataType.getName(), String.valueOf(i))).collect(Collectors.toList());
                    return criteriaBuilder.in(path.get(fieldName)).value(querySet).not();
                }
                return criteriaBuilder.in(path.get(fieldName))
                        .value(input.getValue()).not();
            case "CONTAINS":
                if (dataType.isAssignableFrom(List.class))
                    return criteriaBuilder.isMember(input.getValue(), path.get(fieldName));
                else
                    throw new RuntimeException("Criteria not supported yet");
            case "ISNULL":
                return criteriaBuilder.isNull(path.get(fieldName));
            case "ISNOTNULL":
                return criteriaBuilder.isNotNull(path.get(fieldName));
            default:
                throw new RuntimeException("Operation not supported yet");
        }
    }

    private static LocalDateTime convertFromTenantTimeZone(Path path, String fieldName, LocalDateTime localDateTime) {
        LocalDateTime res = localDateTime;
        try {
            Class<?> clazz = path.getModel().getBindableJavaType();
            Field field = clazz.getDeclaredField(fieldName);
            field.setAccessible(true);
            if(!field.isAnnotationPresent(ExcludeTimeZone.class)) {
                res = LocalTimeZoneHelper.getDateTimeFromUserTimeZone(localDateTime);
            }
        }
        catch (Exception e) {
            // do nothing return back the original datetime
        }
        return res;
    }

    private static <T> Specification<T> getSpecificationFromFiltersWithoutMapping(List<FilterCriteria> filter, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className, Map<String, Class> dataTypeMap) {
        if (filter == null || filter.size() == 0) {
            return null;
        }

        Specification<T> specification = null;

        for (FilterCriteria input : filter) {
            if (input.getInnerFilter() != null && input.getInnerFilter().size() > 0) {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(getSpecificationFromFiltersWithoutMapping(input.getInnerFilter(), null, map, className, dataTypeMap));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(getSpecificationFromFiltersWithoutMapping(input.getInnerFilter(), null, map, className, dataTypeMap));
                    }
                } else {
                    specification =
                            where(getSpecificationFromFiltersWithoutMapping(input.getInnerFilter(), sortRequest, map, className, dataTypeMap));
                }
            } else {
                if (input.getLogicOperator() != null) {
                    if (input.getLogicOperator().equalsIgnoreCase("OR")) {
                        specification = specification.or(createSpecificationWithoutFilter(input.getCriteria(), null, map, className, dataTypeMap));
                    } else if (input.getLogicOperator().equalsIgnoreCase("AND")) {
                        specification = specification.and(createSpecificationWithoutFilter(input.getCriteria(), null, map, className, dataTypeMap));
                    }
                } else {
                    specification =
                            where(createSpecificationWithoutFilter(input.getCriteria(), sortRequest, map, className, dataTypeMap));
                }
            }
        }
        return specification;
    }

    private static <T> Specification<T> createSpecificationWithoutFilter(Criteria input, SortRequest sortRequest, Map<String, Join<Class, T>> map, String className, Map<String, Class> dataTypeMap) {
        return (root, query, criteriaBuilder) -> {
            Path path = root;

            if (!query.getResultType().isAssignableFrom(Long.class) && sortRequest != null && (query.getOrderList() == null || query.getOrderList().size() == 0)) {
                if (sortRequest.getOrder().equalsIgnoreCase("DESC")) {
                    query.orderBy(Arrays.asList(criteriaBuilder.desc(root.get(sortRequest.getFieldName()))));
                } else {
                    query.orderBy(Arrays.asList(criteriaBuilder.asc(root.get(sortRequest.getFieldName()))));
                }
            }
            return createSpecification(dataTypeMap.get(input.getFieldName()), input, path, criteriaBuilder, input.getFieldName());

        };
    }

    private static LocalDateTime covertStringToLocalDate(String date, String pattern) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);
        try {
            return LocalDate.parse(date, formatter).atStartOfDay();
        } catch (Exception e) {
            return LocalDateTime.parse(date,DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        }
    }

    private static Date covertStringToData(String date, String pattern) {

        SimpleDateFormat formatter = new SimpleDateFormat(pattern, Locale.ENGLISH);
        try {
            return formatter.parse(date);
        } catch (ParseException e) {
            return null;
        }
    }
}
