package com.dpw.runner.shipment.services.aspects.v1;

import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.lang.reflect.Field;

@Aspect
@Component
@Slf4j
public class V1Aspect {

  @Autowired
  private ModelMapper modelMapper;

  @Autowired
  private JsonHelper jsonHelper;

  @Before("execution(* com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl+.*(..))")
  public void beforeFindOfV1ServiceImpl(JoinPoint joinPoint) throws RunnerException {
    Object[] args = joinPoint.getArgs();  // Fetching the arguments

    if (args != null && args.length > 0) {
      for (Object arg : args) {
        if (arg instanceof Map<?, ?>) {
          Map<?, ?> argMap = (Map<?, ?>) arg;


          if (argMap.containsKey("Take")) {
            Object takeValue = argMap.get("Take");


            if (takeValue instanceof Integer) {
              int takeInt = (Integer) takeValue;

              if (takeInt == 0 || takeInt > 500) {
                ((Map<Object, Object>) argMap).put("Take", 500);
                log.info("Take field set to 500 for argument: {}", argMap);
              }
            }
          }
        } else if (arg != null) {
          Class<?> clazz = arg.getClass();


          for (Field field : clazz.getDeclaredFields()) {
            if ("Take".equalsIgnoreCase(field.getName())) {
              field.setAccessible(true);

              try {

                Object fieldValue = field.get(arg);

                if (fieldValue instanceof Integer) {
                  int takeValue = (Integer) fieldValue;


                  if (takeValue == 0 || takeValue > 500) {
                    field.set(arg, 500);
                    log.info("Field 'take' modified to 500 for class: {}", clazz.getName());
                  }
                }
              } catch (IllegalAccessException e) {
                log.error("Error accessing field 'take': {}", e.getMessage());
              }
            }
          }
        }
      }
    }
  }

}
