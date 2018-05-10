// https://github.com/eclipse/cdt/blob/master/core/org.eclipse.cdt.core.tests/parser/org/eclipse/cdt/core/parser/tests/scanner/FileCodeReaderFactory.java

package clj_cdt;

import org.eclipse.cdt.core.index.IIndexFileLocation;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.internal.core.parser.IMacroDictionary;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContent;
import org.eclipse.cdt.internal.core.parser.scanner.InternalFileContentProvider;

public class FileCodeReaderFactory extends InternalFileContentProvider {
  private static FileCodeReaderFactory instance;

  private FileCodeReaderFactory() {}

  @Override
  public InternalFileContent getContentForInclusion(String path, IMacroDictionary macroDictionary) {
    //System.out.println("getContentForInclusion: "+path);
    return (InternalFileContent) FileContent.createForExternalFileLocation(path);
  }

  public static FileCodeReaderFactory getInstance() {
    if (instance == null)
      instance = new FileCodeReaderFactory();
    return instance;
  }

  @Override
  public InternalFileContent getContentForInclusion(IIndexFileLocation ifl, String astPath) {
    // not used as a delegate
    return null;
  }
}
